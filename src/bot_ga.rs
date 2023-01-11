use std::fs;
use std::io::{stdin, stdout, Write};
use std::process::Command;

use crate::bot::Bot;
use crate::leg::Leg;
use crate::leg_ga::{evolve_legs, LegParams, LegRecords};

pub struct BotParams {
    pub pop_size: usize,
    pub num_neurons: usize,
    pub init_gens: usize,
    pub iters: usize,
    pub mut_rate: f32,
    pub seed: u64,
}

impl BotParams {
    fn label(&self) -> String {
        format!(
            "Bot_{}_{}_{}_{}_{}_{}",
            self.pop_size, self.num_neurons, self.init_gens, self.iters, self.mut_rate, self.seed
        )
    }

    pub fn init_bot_records(&self) -> BotRecords {
        // let leg_lists = transpose(&vec![self.num_neurons; self.pop_size]
        //     .iter()
        //     .map(|&num_neurons| make_rand_bot(num_neurons).0)
        //     .collect());
        // let fit_lists = leg_lists.iter().map(|legs| legs.iter().map(|leg| leg.fit(LegParams::from(self).iters)).collect()).collect();
        let leg_params = LegParams::from(self);
        let leg_record_list: Vec<LegRecords> = vec![leg_params; 6]
            .iter()
            .map(|params| {
                let mut records = params.init_leg_records();
                records.compute_leg_ga(params, self.init_gens)
            })
            .collect();
        let leg_lists = leg_record_list
            .iter()
            .map(|records| records.legs.clone())
            .collect();
        let bots: Vec<Bot> = transpose(&leg_lists)
            .iter()
            .map(|legs| Bot(legs.clone()))
            .collect();
        let fits: Vec<f32> = bots.iter().map(|bot| bot.fit(self.iters)).collect();
        let max_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
        let max_index = fits.iter().position(|&fit| fit == max_fit).unwrap();
        let max_bot = bots[max_index].clone();
        let avg_fit = fits.iter().sum::<f32>() / fits.len() as f32;
        let leg_lists = transpose(&bots.iter().map(|bot| bot.0.clone()).collect());
        let fit_lists = transpose(&fits.iter().map(|&fit| vec![fit; 6]).collect());
        BotRecords {
            max_bots: vec![max_bot.clone()],
            max_fits: vec![max_fit],
            best_bot: max_bot,
            best_fits: vec![max_fit],
            avg_fits: vec![avg_fit],
            leg_lists,
            fit_lists,
        }
    }
}

impl From<&BotParams> for LegParams {
    fn from(params: &BotParams) -> LegParams {
        LegParams {
            pop_size: params.pop_size,
            num_neurons: params.num_neurons,
            iters: params.iters,
            mut_rate: 1e-2,
            seed: params.seed,
        }
    }
}

#[derive(Clone)]
pub struct BotRecords {
    pub max_bots: Vec<Bot>,
    pub max_fits: Vec<f32>,
    best_bot: Bot,
    pub best_fits: Vec<f32>,
    pub avg_fits: Vec<f32>,
    leg_lists: Vec<Vec<Leg>>,
    fit_lists: Vec<Vec<f32>>,
}

impl BotRecords {
    pub fn compute_bot_ga(&mut self, params: &BotParams, gens: usize) -> BotRecords {
        for _ in 0..gens {
            self.leg_lists = self
                .leg_lists
                .iter()
                .zip(self.fit_lists.iter())
                .map(|(legs, fits)| evolve_legs(&legs, &fits, &LegParams::from(params)))
                .collect();
            let bots: Vec<Bot> = transpose(&self.leg_lists)
                .iter()
                .map(|legs| Bot(legs.clone()))
                .collect();
            let fits: Vec<f32> = bots.iter().map(|bot| bot.fit(params.iters)).collect();
            self.fit_lists = transpose(&fits.iter().map(|&fit| vec![fit; 6]).collect());
            let best_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
            let best_index = fits.iter().position(|&fit| fit == best_fit).unwrap();
            self.best_bot = bots[best_index].clone();
            self.best_fits.push(best_fit);
            let prev_max_fit = self.max_fits.iter().last().unwrap();
            if best_fit > *prev_max_fit {
                self.max_bots.push(bots[best_index].clone());
                self.max_fits.push(best_fit);
            } else {
                self.max_fits.push(*prev_max_fit);
            }
            self.avg_fits
                .push(fits.iter().sum::<f32>() / fits.len() as f32);
        }
        self.clone()
    }

    pub fn plot(&self, params: &BotParams, setting: &str) -> () {
        let gens = if setting == "default" {
            self.best_fits.len()
        } else if setting == "evo" {
            print!("\nEnter the generation you would like to plot up to: ");
            stdout().flush().unwrap();
            let mut input = String::new();
            stdin().read_line(&mut input).expect("failed to readline");
            input.parse::<usize>().unwrap() + 1
        } else {
            print!("\nEnter desired generation of bot: ");
            stdout().flush().unwrap();
            let mut input = String::new();
            stdin().read_line(&mut input).expect("failed to readline");
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
                let (xs, ys) = positions.iter().map(|pos| *pos).unzip();
                vec![xs, ys]
            })
            .collect();
        let (xs, ys): (Vec<f32>, Vec<f32>) = bot.path(params.iters).iter().map(|pos| *pos).unzip();
        let bot_path = vec![xs, ys];
        let data = format!(
            "{:?}\n{:?}\n{:?}\n{:?}\n{:?}",
            self.max_fits, self.best_fits, self.avg_fits, position_lists, bot_path
        );
        fs::write("datafile.txt", data).expect("Unable to write file");
        Command::new("python")
            .args([
                "plot_bot_records.py",
                &params.label(),
                setting,
                &(gens - 1).to_string(),
            ])
            .spawn()
            .expect("Call to plot_bot_recrods.py failed.");
    }
}

fn transpose<T>(matrix: &Vec<Vec<T>>) -> Vec<Vec<T>>
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
    for i in 0..matrix.len() {
        for j in 0..matrix[i].len() {
            trans_matrix[j][i] = matrix[i][j].clone();
        }
    }
    trans_matrix
}
