use itertools::iterate;

use crate::bot_env::BotEnv;
use crate::leg::{make_rand_leg, Leg};
use crate::leg_env::LegEnv;

#[derive(Clone, Debug)]
pub struct Bot(pub Vec<Leg>);

pub fn _make_rand_bot(num_neurons: usize) -> Bot {
    let legs = vec![num_neurons; 6]
        .iter()
        .map(|&num_neurons| make_rand_leg(num_neurons))
        .collect();
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
        let init_envs = vec![LegEnv::default(); 6];
        iterate((self.clone(), init_envs), |(bot, envs)| bot.step(envs))
            .take(iters + 1)
            .map(|(_, envs)| envs.iter().map(|env| (env.x_pos, env.y_pos)).collect())
            .collect::<Vec<Vec<(i16, i16)>>>()
    }

    fn step(&self, leg_envs: &Vec<LegEnv>) -> (Bot, Vec<LegEnv>) {
        let sens_value_lists: Vec<[i16; 3]> = leg_envs.iter().map(|env| env.sens_values).collect();
        let new_value_lists: Vec<Vec<i16>> = self
            .0
            .iter()
            .enumerate()
            .map(|(leg_index, leg)| {
                let accums = leg.accumulate_for_bot(&sens_value_lists, leg_index);
                leg.activate(&accums)
            })
            .collect();
        let upd_legs = self
            .0
            .iter()
            .zip(new_value_lists.iter())
            .map(|(leg, values)| leg.update_values(&values))
            .collect();
        let upd_envs = leg_envs
            .iter()
            .zip(new_value_lists.iter())
            .map(|(env, values)| env.act(values[0] as usize, values[1] as usize))
            .collect();
        (Bot(upd_legs), upd_envs)
    }
}
