use fastrand;

use crate::chromosome::{Chromable, LegChrom};
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
    pub fn init_leg_records(&self) -> LegRecords {
        let legs: Vec<Leg> = vec![self.num_neurons; self.pop_size]
            .iter()
            .map(|&num_neurons| make_rand_leg(num_neurons))
            .collect();
        let fits: Vec<f32> = legs.iter().map(|leg| leg.fit(self.iters)).collect();
        let max_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
        let max_index = fits.iter().position(|&fit| fit == max_fit).unwrap();
        let max_leg = legs[max_index].clone();
        let avg_fit = fits.iter().sum::<f32>() / fits.len() as f32;
        LegRecords {
            max_legs: vec![max_leg],
            max_fits: vec![max_fit],
            avg_fits: vec![avg_fit],
            legs,
            fits,
        }
    }
}

#[derive(Clone)]
pub struct LegRecords {
    pub max_legs: Vec<Leg>,
    pub max_fits: Vec<f32>,
    avg_fits: Vec<f32>,
    pub legs: Vec<Leg>,
    pub fits: Vec<f32>,
}

impl LegRecords {
    pub fn compute_leg_ga(&mut self, params: &LegParams, gens: usize) -> LegRecords {
        for _ in 0..gens {
            self.legs = evolve_legs(&self.legs, &self.fits, params);
            self.fits = self.legs.iter().map(|leg| leg.fit(params.iters)).collect();
            let max_fit = *self.fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
            self.max_fits.push(max_fit);
            let max_index = self.fits.iter().position(|&fit| fit == max_fit).unwrap();
            self.max_legs.push(self.legs[max_index].clone());
            self.avg_fits
                .push(self.fits.iter().sum::<f32>() / self.fits.len() as f32);
        }
        self.clone()
    }
}

pub fn evolve_legs(legs: &Vec<Leg>, fits: &Vec<f32>, params: &LegParams) -> Vec<Leg> {
    let max_fit = *fits.iter().max_by(|a, b| a.total_cmp(b)).unwrap();
    let elite_index = fits.iter().position(|&fit| fit == max_fit).unwrap();
    let elite_leg = legs[elite_index].clone();
    let mut child_legs = vec![];
    for _ in 0..params.pop_size - 1 {
        let child = create_leg_child(legs, fits, params.mut_rate);
        child_legs.push(child);
    }
    let n = fastrand::usize(0..child_legs.len());
    child_legs.insert(n, elite_leg);
    child_legs
}

fn create_leg_child(legs: &Vec<Leg>, fits: &Vec<f32>, mut_rate: f32) -> Leg {
    let parents = select(2, legs.clone(), fits.clone());
    let child_chrom = LegChrom::from(&parents[0])
        .cross(&LegChrom::from(&parents[1]))
        .mutate(mut_rate);
    Leg::from(&child_chrom)
}
