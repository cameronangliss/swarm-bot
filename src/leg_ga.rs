use fastrand;
use std::fs;
use std::io::{stdin, stdout, Write};
use std::iter::repeat_with;
use std::process::Command;

use crate::chromosome::LegChrom;
use crate::leg::{make_rand_leg, select, Leg};

#[derive(Clone)]
pub struct LegParams {
    pub pop_size: usize,
    pub num_neurons: usize,
    pub iters: usize,
    pub mut_rate: f32,
    pub seed: u64,
}

impl LegParams {
    fn label(&self) -> String {
        format!(
            "Leg_{}_{}_{}_{:e}_{}",
            self.pop_size, self.num_neurons, self.iters, self.mut_rate, self.seed
        )
    }

    pub fn init_leg_records(&self) -> LegRecords {
        let legs: Vec<Leg> = repeat_with(|| make_rand_leg(self.num_neurons))
            .take(self.pop_size)
            .collect();
        let fits: Vec<f32> = legs.iter().map(|leg| leg.fit(self.iters)).collect();
        let best_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
        let best_index = fits.iter().position(|&fit| fit == best_fit).unwrap();
        let best_leg = legs[best_index].clone();
        let avg_fit = fits.iter().sum::<f32>() / fits.len() as f32;
        LegRecords {
            best_legs: vec![best_leg],
            best_fits: vec![best_fit],
            avg_fits: vec![avg_fit],
            legs,
            fits,
        }
    }
}

#[derive(Clone)]
pub struct LegRecords {
    pub best_legs: Vec<Leg>,
    pub best_fits: Vec<f32>,
    avg_fits: Vec<f32>,
    pub legs: Vec<Leg>,
    pub fits: Vec<f32>,
}

impl LegRecords {
    pub fn compute_leg_ga(&mut self, params: &LegParams, gens: usize) {
        for _ in 0..gens {
            self.legs = evolve_legs(&self.legs, &self.fits, params);
            self.fits = self.legs.iter().map(|leg| leg.fit(params.iters)).collect();
            let best_fit = *self.fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
            self.best_fits.push(best_fit);
            let best_index = self.fits.iter().position(|&fit| fit == best_fit).unwrap();
            let best_leg = self.legs[best_index].clone();
            self.best_legs.push(best_leg);
            let avg_fit = self.fits.iter().sum::<f32>() / self.fits.len() as f32;
            self.avg_fits.push(avg_fit);
        }
    }

    pub fn plot(&self, params: &LegParams, setting: &str) {
        let gens = if setting == "default" {
            self.best_fits.len()
        } else if setting == "evo" {
            print!("\nEnter the generation you would like to plot up to: ");
            stdout().flush().unwrap();
            let mut input = String::new();
            stdin().read_line(&mut input).unwrap();
            input.parse::<usize>().unwrap() + 1
        } else {
            print!("\nEnter desired generation of leg: ");
            stdout().flush().unwrap();
            let mut input = String::new();
            stdin().read_line(&mut input).unwrap();
            input.parse::<usize>().unwrap() + 1
        };
        let leg = self.best_legs.iter().last().unwrap();
        let (xs, ys): (Vec<i16>, Vec<i16>) = leg.test(params.iters).iter().map(|pos| *pos).unzip();
        let positions = vec![xs, ys];
        let data = format!("{:?}\n{:?}\n{:?}", self.best_fits, self.avg_fits, positions);
        fs::write("datafile.txt", data).unwrap();
        Command::new("python")
            .args([
                "plot_leg_records.py",
                &params.label(),
                setting,
                &(gens - 1).to_string(),
            ])
            .spawn()
            .unwrap();
    }
}

pub fn evolve_legs(legs: &Vec<Leg>, fits: &Vec<f32>, params: &LegParams) -> Vec<Leg> {
    let max_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
    let elite_index = fits.iter().position(|&fit| fit == max_fit).unwrap();
    let elite_leg = legs[elite_index].clone();
    let mut child_legs: Vec<Leg> = repeat_with(|| create_leg_child(legs, fits, params.mut_rate))
        .take(params.pop_size - 1)
        .collect();
    let n = fastrand::usize(0..=child_legs.len());
    child_legs.insert(n, elite_leg);
    child_legs
}

fn create_leg_child(legs: &Vec<Leg>, fits: &Vec<f32>, mut_rate: f32) -> Leg {
    let parents = select(2, legs, fits);
    let chrom1 = LegChrom::from(&parents[0]);
    let chrom2 = LegChrom::from(&parents[1]);
    let child_chrom = chrom1.cross(&chrom2).mutate(mut_rate);
    Leg::from(&child_chrom)
}
