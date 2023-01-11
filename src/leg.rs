use itertools::iterate;

use crate::leg_env::LegEnv;
use crate::neuron::{make_rand_neuron, Neuron};

#[derive(Clone, Debug, Default)]
pub struct Leg(pub Vec<Neuron>);

pub fn make_rand_leg(num_neurons: usize) -> Leg {
    let nrns = [vec![3, 3], vec![5; num_neurons - 2]]
        .concat()
        .iter()
        .map(|num_sensor_weights| make_rand_neuron(num_neurons, *num_sensor_weights))
        .collect();
    Leg(nrns)
}

impl Leg {
    pub fn fit(&self, iters: usize) -> f32 {
        let positions = self.test(iters);
        let mut distance = 0.0;
        for i in 0..positions.len() - 1 {
            let (x1, _) = positions[i];
            let (x2, y2) = positions[i + 1];
            distance += (x2 - x1) as f32 / (1 + y2) as f32;
        }
        distance
    }

    fn test(&self, iters: usize) -> Vec<(i16, i16)> {
        let init_env = LegEnv::default();
        iterate((self.clone(), init_env), |(leg, env)| leg.step(env))
            .take(iters + 1)
            .map(|(_, env)| (env.x_pos, env.y_pos))
            .collect()
    }

    fn step(&self, env: &LegEnv) -> (Leg, LegEnv) {
        let accums = self.accumulate_for_leg(&env.sens_values);
        let activs = self.activate(&accums);
        let upd_leg = self.update_values(&activs);
        let upd_env = env.act(activs[0] as usize, activs[1] as usize);
        (upd_leg, upd_env)
    }

    fn accumulate_for_leg(&self, sens_values: &Vec<i16>) -> Vec<i16> {
        let values = self.0.iter().map(|neuron| neuron.value).collect();
        let out_accums: Vec<i16> = self
            .0
            .iter()
            .take(2)
            .map(|neuron| neuron.accumulate(&values, sens_values))
            .collect();
        let hid_accums = self.0[2..]
            .iter()
            .map(|neuron| neuron.accumulate(&values, &vec![]))
            .collect();
        [out_accums, hid_accums].concat()
    }

    pub fn accumulate_for_bot(
        &self,
        mut sens_value_lists: Vec<Vec<i16>>,
        leg_index: usize,
    ) -> Vec<i16> {
        let values = self.0.iter().map(|neuron| neuron.value).collect();
        let out_sens_values = &sens_value_lists[leg_index];
        let out_accums: Vec<i16> = self
            .0
            .iter()
            .take(2)
            .map(|neuron| neuron.accumulate(&values, &out_sens_values))
            .collect();
        sens_value_lists.remove(leg_index);
        let hid_sens_values = sens_value_lists
            .iter()
            .map(|sens_values| sens_values[2])
            .collect();
        let hid_accums = self.0[2..]
            .iter()
            .map(|neuron| neuron.accumulate(&values, &hid_sens_values))
            .collect();
        [out_accums, hid_accums].concat()
    }

    pub fn activate(&self, accs: &Vec<i16>) -> Vec<i16> {
        self.0
            .iter()
            .zip(accs.iter())
            .map(|(neuron, &acc)| neuron.activate(acc))
            .collect()
    }

    pub fn update_values(&self, values: &Vec<i16>) -> Leg {
        let neurons = self
            .0
            .iter()
            .zip(values.iter())
            .map(|(neuron, &value)| Neuron {
                value,
                ..neuron.clone()
            })
            .collect();
        Leg(neurons)
    }
}

pub fn select(num_select: usize, mut legs: Vec<Leg>, mut fits: Vec<f32>) -> Vec<Leg> {
    if num_select <= 0 {
        vec![]
    } else if num_select >= legs.len() {
        legs.to_vec()
    } else {
        let mut selected_legs = vec![];
        for _ in 0..num_select {
            let min_fit = fits.iter().min_by(|a, b| a.total_cmp(b)).unwrap();
            let shifted_fits: Vec<f32> = fits.iter().map(|fit| fit - min_fit + 1.0).collect();
            let normed_fits: Vec<f32> = shifted_fits
                .iter()
                .map(|shifted_fit| shifted_fit / shifted_fits.iter().sum::<f32>())
                .collect();
            let prob_list = normed_fits[..normed_fits.len() - 1]
                .iter()
                .scan(0.0, |acc, &normed_fit| {
                    *acc += normed_fit;
                    Some(*acc)
                })
                .collect::<Vec<f32>>();
            let r = fastrand::f32();
            let index = prob_list
                .iter()
                .filter(|prob| **prob < r)
                .map(|prob| *prob)
                .collect::<Vec<f32>>()
                .len();
            selected_legs.push(legs[index].clone());
            legs.remove(index);
            fits.remove(index);
        }
        selected_legs
    }
}
