use fastrand;
use std::iter::repeat;

#[derive(Clone, Debug)]
pub struct Neuron {
    pub value: i16,
    pub trans: i16,
    pub stretch: i16,
    pub weights: Vec<i16>,
    pub sens_weights: Vec<i16>,
}

pub fn make_rand_neuron(num_weights: usize, num_sens_weights: usize) -> Neuron {
    let value = fastrand::i16(0..=15);
    let trans = fastrand::i16(-511..=511);
    let stretch = fastrand::i16(-255..=255);
    let weights = repeat(-15..=15)
        .take(num_weights)
        .map(|range| fastrand::i16(range))
        .collect();
    let sens_weights = repeat(-15..=15)
        .take(num_sens_weights)
        .map(|range| fastrand::i16(range))
        .collect();
    Neuron {
        value,
        trans,
        stretch,
        weights,
        sens_weights,
    }
}

impl Neuron {
    pub fn accumulate(&self, values: &Vec<i16>, sens_values: &Vec<i16>) -> i16 {
        let inter_neuron_acc = values
            .iter()
            .zip(self.weights.iter())
            .fold(0, |acc, (v, w)| acc + v * w);
        let sensor_acc = sens_values
            .iter()
            .zip(self.sens_weights.iter())
            .fold(0, |acc, (sens_v, sens_w)| acc + sens_v * sens_w);
        inter_neuron_acc + sensor_acc
    }
    pub fn activate(&self, acc: i16) -> i16 {
        let x = (self.trans - acc) as f32 / (self.stretch as f32);
        (15.0 / (1.0 + f32::exp(x))).round() as i16
    }
}
