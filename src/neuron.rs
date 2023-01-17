use fastrand;

#[derive(Clone, Debug, PartialEq)]
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
    let mut weights = vec![];
    for _ in 0..num_weights {
        weights.push(fastrand::i16(-15..=15));
    }
    let mut sens_weights = vec![];
    for _ in 0..num_sens_weights {
        sens_weights.push(fastrand::i16(-15..=15));
    }
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
        if self.stretch == 0 {
            if acc < self.trans {
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