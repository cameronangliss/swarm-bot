use fastrand;
use ndarray::arr1;
use serde_derive::{Deserialize, Serialize};
use serde_json;
use std::fs;
use std::io::{stdin, stdout, Write};
use std::iter::repeat_with;
use std::process::Command;

use crate::bot::{make_rand_bot, Bot};
use crate::leg::Leg;
use crate::leg_ga::{evolve_legs, LegParams};

pub struct BotParams {
    pub pop_size: usize,
    pub num_neurons: usize,
    pub iters: usize,
    pub mut_rate: f32,
    pub min_std_dev: f32,
    pub sample_size: usize,
    pub seed: u64,
}

impl BotParams {
    pub fn label(&self) -> String {
        format!(
            "Bot_{}_{}_{}_{:e}_{}_{}_{}",
            self.pop_size,
            self.num_neurons,
            self.iters,
            self.mut_rate,
            self.min_std_dev,
            self.sample_size,
            self.seed
        )
    }

    pub fn load_records(&self) -> BotRecords {
        let filepath = format!("runs/{}.txt", self.label());
        let save_str = fs::read_to_string(filepath).unwrap();
        serde_json::from_str(&save_str).unwrap()
    }

    pub fn init_bot_records(&self) -> BotRecords {
        let bots: Vec<Bot> = repeat_with(|| make_rand_bot(self.num_neurons))
            .take(self.pop_size)
            .collect();
        let leg_lists = transpose(&bots.iter().map(|bot| bot.0.clone()).collect());
        let fits: Vec<f32> = bots.iter().map(|bot| bot.fit(self.iters)).collect();
        let best_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
        let best_index = fits.iter().position(|&fit| fit == best_fit).unwrap();
        let best_bot = bots[best_index].clone();
        let avg_fit = fits.iter().sum::<f32>() / fits.len() as f32;
        BotRecords {
            max_bots: vec![best_bot.clone()],
            max_fits: vec![best_fit],
            best_bot,
            best_fits: vec![best_fit],
            avg_fits: vec![avg_fit],
            leg_lists,
            fit_lists: vec![fits; 6],
            mut_rate: self.mut_rate,
        }
    }
}

impl From<&BotParams> for LegParams {
    fn from(params: &BotParams) -> LegParams {
        LegParams {
            pop_size: params.pop_size,
            num_neurons: params.num_neurons,
            iters: params.iters,
            mut_rate: params.mut_rate,
            seed: params.seed,
        }
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct BotRecords {
    pub max_bots: Vec<Bot>,
    pub max_fits: Vec<f32>,
    best_bot: Bot,
    pub best_fits: Vec<f32>,
    pub avg_fits: Vec<f32>,
    pub leg_lists: Vec<Vec<Leg>>,
    fit_lists: Vec<Vec<f32>>,
    pub mut_rate: f32,
}

impl BotRecords {
    pub fn compute_bot_ga(&mut self, params: &BotParams, gens: usize) {
        for _ in 0..gens {
            let leg_params = LegParams::from(&BotParams {
                mut_rate: self.mut_rate,
                ..*params
            });
            self.leg_lists = self
                .leg_lists
                .iter()
                .zip(self.fit_lists.iter())
                .map(|(legs, fits)| evolve_legs(legs, fits, &leg_params))
                .collect();
            let bots: Vec<Bot> = transpose(&self.leg_lists)
                .iter()
                .map(|legs| Bot(legs.clone()))
                .collect();
            let fits: Vec<f32> = bots.iter().map(|bot| bot.fit(params.iters)).collect();
            self.fit_lists = vec![fits.clone(); 6];
            let best_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
            self.best_fits.push(best_fit);
            let best_index = fits.iter().position(|&fit| fit == best_fit).unwrap();
            self.best_bot = bots[best_index].clone();
            let prev_max_fit = self.max_fits.iter().last().unwrap();
            if best_fit > *prev_max_fit {
                self.max_bots.push(bots[best_index].clone());
                self.max_fits.push(best_fit);
            } else {
                self.max_fits.push(*prev_max_fit);
            }
            let avg_fit = fits.iter().sum::<f32>() / fits.len() as f32;
            self.avg_fits.push(avg_fit);
            if self.best_fits.len() % params.sample_size == 0 {
                self.update_mut_rate(params);
            }
        }
    }

    fn update_mut_rate(&mut self, params: &BotParams) {
        let sample: Vec<f32> = self
            .best_fits
            .iter()
            .rev()
            .take(params.sample_size)
            .copied()
            .collect();
        let std_dev = arr1(&sample).std(0.0);
        if std_dev >= params.min_std_dev {
            self.mut_rate = params.mut_rate;
        } else {
            self.mut_rate *= 2.0;
        }
    }

    pub fn plot(&self, params: &BotParams, setting: &str) {
        let gens = if setting == "default" {
            self.best_fits.len()
        } else if setting == "evo" {
            print!("\nEnter the generation you would like to plot up to: ");
            stdout().flush().unwrap();
            let mut input = String::new();
            stdin().read_line(&mut input).unwrap();
            input.parse::<usize>().unwrap() + 1
        } else {
            print!("\nEnter desired generation of bot: ");
            stdout().flush().unwrap();
            let mut input = String::new();
            stdin().read_line(&mut input).unwrap();
            input.parse::<usize>().unwrap() + 1
        };
        let bot = if setting == "max" {
            self.max_bots.iter().last().unwrap()
        } else {
            &self.best_bot
        };
        let position_lists: Vec<Vec<Vec<i16>>> = transpose(&bot.test(params.iters))
            .iter()
            .map(|positions| {
                let (xs, ys) = positions.iter().copied().unzip();
                vec![xs, ys]
            })
            .collect();
        let (xs, ys): (Vec<f32>, Vec<f32>) = bot.path(params.iters).iter().copied().unzip();
        let bot_path = vec![xs, ys];
        let data = format!(
            "{:?}\n{:?}\n{:?}\n{:?}\n{:?}",
            self.max_fits, self.best_fits, self.avg_fits, position_lists, bot_path
        );
        fs::write("datafile.txt", data).unwrap();
        Command::new("python3")
            .args([
                "plot_bot_records.py",
                &params.label(),
                setting,
                &(gens - 1).to_string(),
            ])
            .spawn()
            .unwrap();
    }

    pub fn save(&self, params: &BotParams) {
        let filepath = format!("runs/{}.txt", params.label());
        let save_str = serde_json::to_string(&(self, fastrand::get_seed())).unwrap();
        fs::write(filepath, save_str).unwrap();
    }
}

pub fn transpose<T>(matrix: &Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
    T: Default,
{
    let m = matrix.len();
    let n = matrix[0].len();
    let mut trans_matrix: Vec<Vec<T>> = vec![];
    for _ in 0..n {
        trans_matrix.push(vec![T::default(); m])
    }
    for (i, row) in matrix.iter().enumerate() {
        for (j, item) in row.iter().enumerate() {
            trans_matrix[j][i] = item.clone();
        }
    }
    trans_matrix
}
