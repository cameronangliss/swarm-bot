use serde_derive::{Deserialize, Serialize};
use std::iter::repeat_with;

use crate::bot_env::BotEnv;
use crate::leg::{make_rand_leg, Leg};
use crate::leg_env::LegEnv;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Bot(pub Vec<Leg>);

pub fn make_rand_bot(num_neurons: usize) -> Bot {
    let legs = repeat_with(|| make_rand_leg(num_neurons)).take(6).collect();
    Bot(legs)
}

impl Bot {
    pub fn fit(&self, iter: usize) -> f32 {
        self.path(iter).last().unwrap().0
    }

    pub fn path(&self, iter: usize) -> Vec<(f32, f32)> {
        let position_lists = self.test(iter);
        let mut env = BotEnv::default();
        env.calc_path(&position_lists)
    }

    pub fn test(&self, iters: usize) -> Vec<Vec<(i16, i16)>> {
        let mut bot = self.clone();
        let mut envs = vec![LegEnv::default(); 6];
        let mut position_lists = vec![envs.iter().map(|env| (env.x_pos, env.y_pos)).collect()];
        for _ in 0..iters {
            bot.step(&mut envs);
            position_lists.push(envs.iter().map(|env| (env.x_pos, env.y_pos)).collect());
        }
        position_lists
    }

    fn step(&mut self, leg_envs: &mut Vec<LegEnv>) {
        let sens_value_lists: Vec<[i16; 3]> = leg_envs.iter().map(|env| env.sens_values).collect();
        let accum_lists = self.accumulate(&sens_value_lists);
        let new_value_lists = self.activate(&accum_lists);
        self.update_values(&new_value_lists);
        leg_envs
            .iter_mut()
            .zip(new_value_lists.iter())
            .for_each(|(env, values)| env.act(values[0] as usize, values[1] as usize));
    }

    fn accumulate(&self, sens_value_lists: &Vec<[i16; 3]>) -> Vec<Vec<i16>> {
        self.0
            .iter()
            .enumerate()
            .map(|(leg_index, leg)| leg.accumulate_for_bot(sens_value_lists, leg_index))
            .collect()
    }

    fn activate(&self, accum_lists: &Vec<Vec<i16>>) -> Vec<Vec<i16>> {
        self.0
            .iter()
            .zip(accum_lists.iter())
            .map(|(leg, accums)| leg.activate(accums))
            .collect()
    }

    fn update_values(&mut self, value_lists: &Vec<Vec<i16>>) {
        self.0
            .iter_mut()
            .zip(value_lists.iter())
            .for_each(|(leg, values)| leg.update_values(&values));
    }
}
