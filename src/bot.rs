use itertools::iterate;

use crate::bot_env::BotEnv;
use crate::leg::Leg;
use crate::leg_env::LegEnv;

#[derive(Clone, Debug)]
pub struct Bot(pub Vec<Leg>);

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
        let legs = &self.0;
        let sens_value_lists: Vec<Vec<i16>> =
            leg_envs.iter().map(|env| env.sens_values.clone()).collect();
        let accum_lists: Vec<Vec<i16>> = legs
            .iter()
            .zip(0..6)
            .map(|(leg, leg_index)| leg.accumulate_for_bot(sens_value_lists.clone(), leg_index))
            .collect();
        let activ_lists: Vec<Vec<i16>> = legs
            .iter()
            .zip(accum_lists)
            .map(|(leg, accums)| leg.activate(&accums))
            .collect();
        let upd_legs = legs
            .iter()
            .zip(activ_lists.iter())
            .map(|(leg, activs)| leg.update_values(&activs))
            .collect();
        let upd_envs = leg_envs
            .iter()
            .zip(activ_lists.iter())
            .map(|(env, activs)| env.act(activs[0] as usize, activs[1] as usize))
            .collect();
        (Bot(upd_legs), upd_envs)
    }
}
