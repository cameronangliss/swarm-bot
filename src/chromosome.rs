use fastrand;

use crate::leg::Leg;
use crate::neuron::Neuron;

///////////////////////////////////////////////////////////////////////////////
/// Leg Chromosome

#[derive(Debug)]
pub struct LegChrom(pub Vec<NeuronChrom>);

impl LegChrom {
    pub fn cross(&self, chrom: &LegChrom) -> LegChrom {
        let neuron_chroms = self
            .0
            .iter()
            .zip(chrom.0.iter())
            .map(|(neuron_chrom1, neuron_chrom2)| neuron_chrom1.cross(neuron_chrom2))
            .collect();
        LegChrom(neuron_chroms)
    }

    pub fn mutate(&self, mut_rate: f32) -> LegChrom {
        let neuron_chroms = self
            .0
            .iter()
            .map(|neuron_chrom| neuron_chrom.mutate(mut_rate))
            .collect();
        LegChrom(neuron_chroms)
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Leg <--> Chromosome Conversions

impl From<&LegChrom> for Leg {
    fn from(leg_chrom: &LegChrom) -> Leg {
        let neurons = leg_chrom.0.iter().map(Neuron::from).collect();
        Leg(neurons)
    }
}

impl From<&Leg> for LegChrom {
    fn from(leg: &Leg) -> LegChrom {
        let neuron_chroms = leg.0.iter().map(NeuronChrom::from).collect();
        LegChrom(neuron_chroms)
    }
}

///////////////////////////////////////////////////////////////////////////////
/// Neuron Chromosome

#[derive(Debug)]
pub struct NeuronChrom(
    pub String,
    pub String,
    pub String,
    pub Vec<String>,
    pub Vec<String>,
);

impl NeuronChrom {
    pub fn cross(&self, chrom: &NeuronChrom) -> NeuronChrom {
        let value_bin = cross_bins(&self.0, &chrom.0);
        let trans_bin = cross_bins(&self.1, &chrom.1);
        let stretch_bin = cross_bins(&self.2, &chrom.2);
        let weight_bins = self
            .3
            .iter()
            .zip(chrom.3.iter())
            .map(|(weight_bin1, weight_bin2)| cross_bins(weight_bin1, weight_bin2))
            .collect();
        let sens_weight_bins = self
            .4
            .iter()
            .zip(chrom.4.iter())
            .map(|(sens_weight_bin1, sens_weight_bin2)| {
                cross_bins(sens_weight_bin1, sens_weight_bin2)
            })
            .collect();
        NeuronChrom(
            value_bin,
            trans_bin,
            stretch_bin,
            weight_bins,
            sens_weight_bins,
        )
    }

    fn mutate(&self, mut_rate: f32) -> NeuronChrom {
        let value_bin = mutate_bin(&self.0, mut_rate);
        let trans_bin = mutate_bin(&self.1, mut_rate);
        let stretch_bin = mutate_bin(&self.2, mut_rate);
        let weight_bins = self
            .3
            .iter()
            .map(|weight_bin| mutate_bin(weight_bin, mut_rate))
            .collect();
        let sens_weight_bins = self
            .4
            .iter()
            .map(|sens_weight_bin| mutate_bin(sens_weight_bin, mut_rate))
            .collect();
        NeuronChrom(
            value_bin,
            trans_bin,
            stretch_bin,
            weight_bins,
            sens_weight_bins,
        )
    }
}

fn cross_bins(bin1: &str, bin2: &str) -> String {
    if fastrand::bool() {
        bin1.to_string()
    } else {
        bin2.to_string()
    }
}

fn mutate_bin(bin: &str, mut_rate: f32) -> String {
    bin.chars()
        .map(|bit| {
            let r = fastrand::f32();
            let flipped_bit = if bit == '0' { '1' } else { '0' };
            if r < mut_rate {
                flipped_bit
            } else {
                bit
            }
        })
        .collect()
}

///////////////////////////////////////////////////////////////////////////////
/// Neuron <--> Chromosome Conversions

impl From<&NeuronChrom> for Neuron {
    fn from(nrn_chrom: &NeuronChrom) -> Neuron {
        let value = usize::from_str_radix(&nrn_chrom.0, 2).unwrap();
        let trans = from_signed_bin(&nrn_chrom.1);
        let stretch = from_signed_bin(&nrn_chrom.2);
        let weights = nrn_chrom
            .3
            .iter()
            .map(|weight_bin| from_signed_bin(weight_bin))
            .collect();
        let sens_weights = nrn_chrom
            .4
            .iter()
            .map(|sens_weight_bin| from_signed_bin(sens_weight_bin))
            .collect();
        Neuron {
            value,
            trans,
            stretch,
            weights,
            sens_weights,
        }
    }
}

impl From<&Neuron> for NeuronChrom {
    fn from(nrn: &Neuron) -> NeuronChrom {
        let value_bin = format!("{:04b}", nrn.value);
        let trans_bin = to_signed_bin(nrn.trans, 10);
        let stretch_bin = to_signed_bin(nrn.stretch, 9);
        let weight_bins = nrn.weights.iter().map(|&w| to_signed_bin(w, 5)).collect();
        let sens_weight_bins = nrn
            .sens_weights
            .iter()
            .map(|&sw| to_signed_bin(sw, 5))
            .collect();
        NeuronChrom(
            value_bin,
            trans_bin,
            stretch_bin,
            weight_bins,
            sens_weight_bins,
        )
    }
}

// converts integer to binary string with sign-magnitude representation
fn to_signed_bin(x: i16, len: usize) -> String {
    let bin = format!("{:b}", x.abs());
    let pad_len = if len > bin.len() + 1 {
        len - bin.len() - 1
    } else {
        0
    };
    let padding = "0".repeat(pad_len);
    let sign_bit = if x < 0 {
        "1".to_string()
    } else {
        "0".to_string()
    };
    sign_bit + &padding + &bin
}

// converts binary string with sign-magnitude representation to integer
fn from_signed_bin(s: &str) -> i16 {
    let sign_bit = s.chars().next().unwrap();
    let bin: String = s.chars().skip(1).collect();
    let x = i16::from_str_radix(&bin, 2).unwrap();
    if sign_bit == '0' {
        x
    } else {
        -x
    }
}
