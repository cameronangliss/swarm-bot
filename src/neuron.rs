use fastrand;
use serde_derive::{Deserialize, Serialize};
use std::iter::repeat_with;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
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
    let weights = repeat_with(|| fastrand::i16(-15..=15))
        .take(num_weights)
        .collect();
    let sens_weights = repeat_with(|| fastrand::i16(-15..=15))
        .take(num_sens_weights)
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
    pub fn accumulate(&self, values: &[i16], sens_values: &[i16]) -> i16 {
        let inter_neuron_acc = values
            .iter()
            .zip(self.weights.iter())
            .fold(0, |acc, (&v, w)| acc + (v as i16) * w);
        let sensor_acc = sens_values
            .iter()
            .zip(self.sens_weights.iter())
            .fold(0, |acc, (sens_v, sens_w)| acc + sens_v * sens_w);
        inter_neuron_acc + sensor_acc
    }
    pub fn activate(&self, acc: i16) -> i16 {
        if self.stretch == 0 {
            if acc > self.trans {
                15
            } else {
                0
            }
        } else {
            let power = (self.trans - acc) as f32 / (self.stretch as f32);
            (15.0 / (1.0 + power.exp())).round() as i16
        }
    }
}
