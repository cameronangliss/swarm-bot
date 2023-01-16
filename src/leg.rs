use crate::leg_env::LegEnv;
use crate::neuron::{make_rand_neuron, Neuron};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Leg(pub Vec<Neuron>);

pub fn make_rand_leg(num_neurons: usize) -> Leg {
    let neurons = [vec![3, 3], vec![5; num_neurons - 2]]
        .concat()
        .iter()
        .map(|&num_sensor_weights| make_rand_neuron(num_neurons, num_sensor_weights))
        .collect();
    Leg(neurons)
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

    pub fn test(&self, iters: usize) -> Vec<(i16, i16)> {
        let mut leg = self.clone();
        let mut env = LegEnv::default();
        let mut positions = vec![(env.x_pos, env.y_pos)];
        for _ in 0..iters {
            leg.step(&mut env);
            positions.push((env.x_pos, env.y_pos));
        }
        positions
    }

    fn step(&mut self, env: &mut LegEnv) {
        let accums = self.accumulate_for_leg(&env.sens_values);
        let activs = self.activate(&accums);
        self.update_values(&activs);
        env.act(activs[0] as usize, activs[1] as usize);
    }

    fn accumulate_for_leg(&self, sens_values: &[i16; 3]) -> Vec<i16> {
        let values = self.0.iter().map(|neuron| neuron.value).collect();
        let out_accums: Vec<i16> = self
            .0
            .iter()
            .take(2)
            .map(|neuron| neuron.accumulate(&values, &sens_values.to_vec()))
            .collect();
        let hid_accums = self.0[2..]
            .iter()
            .map(|neuron| neuron.accumulate(&values, &vec![]))
            .collect();
        [out_accums, hid_accums].concat()
    }

    pub fn accumulate_for_bot(
        &self,
        sens_value_lists: &Vec<[i16; 3]>,
        leg_index: usize,
    ) -> Vec<i16> {
        let values = self.0.iter().map(|neuron| neuron.value).collect();
        let out_sens_values = sens_value_lists[leg_index].to_vec();
        let out_accums: Vec<i16> = self
            .0
            .iter()
            .take(2)
            .map(|neuron| neuron.accumulate(&values, &out_sens_values))
            .collect();
        let hid_sens_values = [
            sens_value_lists[0..leg_index].to_vec(),
            sens_value_lists[leg_index + 1..].to_vec(),
        ]
        .concat()
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

    pub fn update_values(&mut self, values: &Vec<i16>) {
        self.0
            .iter_mut()
            .zip(values.iter())
            .for_each(|(neuron, &value)| neuron.value = value);
    }
}

pub fn select(num_select: usize, legs: &Vec<Leg>, fits: &Vec<f32>) -> Vec<Leg> {
    if num_select <= 0 {
        vec![]
    } else if num_select >= legs.len() {
        legs.clone()
    } else {
        let mut legs_left = legs.clone();
        let mut fits_left = fits.clone();
        let mut selected_legs = vec![];
        for _ in 0..num_select {
            let min_fit = fits_left.iter().min_by(|a, b| a.total_cmp(b)).unwrap();
            let shifted_fits: Vec<f32> = fits_left.iter().map(|fit| fit - min_fit + 1.0).collect();
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
            selected_legs.push(legs_left[index].clone());
            legs_left.remove(index);
            fits_left.remove(index);
        }
        selected_legs
    }
}
