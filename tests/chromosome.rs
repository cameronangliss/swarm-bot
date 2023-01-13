use swarm_bot::chromosome::*;
use swarm_bot::leg::{make_rand_leg, Leg};
use swarm_bot::neuron::{make_rand_neuron, Neuron};

#[test]
fn leg_cross() {
    let leg1 = make_rand_leg(5);
    let leg2 = make_rand_leg(5);
    let chrom1 = LegChrom::from(&leg1);
    let chrom2 = LegChrom::from(&leg2);
    let chrom3 = chrom1.cross(&chrom2);
    for i in 0..chrom1.0.len() {
        neuron_cross_helper(&chrom1.0[i], &chrom2.0[i], &chrom3.0[i])
    }
}

#[test]
fn neuron_cross() {
    let neuron1 = make_rand_neuron(5, 5);
    let neuron2 = make_rand_neuron(5, 5);
    let chrom1 = NeuronChrom::from(&neuron1);
    let chrom2 = NeuronChrom::from(&neuron2);
    let chrom3 = chrom1.cross(&chrom2);
    neuron_cross_helper(&chrom1, &chrom2, &chrom3)
}

fn neuron_cross_helper(chrom1: &NeuronChrom, chrom2: &NeuronChrom, chrom3: &NeuronChrom) {
    let NeuronChrom(val_bin1, trans_bin1, stretch_bin1, weight_bins1, sens_weight_bins1) = chrom1;
    let NeuronChrom(val_bin2, trans_bin2, stretch_bin2, weight_bins2, sens_weight_bins2) = chrom2;
    let NeuronChrom(val_bin3, trans_bin3, stretch_bin3, weight_bins3, sens_weight_bins3) = chrom3;
    for i in 0..val_bin1.len() {
        assert!(
            val_bin1.chars().nth(i).unwrap() == val_bin3.chars().nth(i).unwrap()
                || val_bin2.chars().nth(i).unwrap() == val_bin3.chars().nth(i).unwrap()
        )
    }
    for i in 0..trans_bin1.len() {
        assert!(
            trans_bin1.chars().nth(i).unwrap() == trans_bin3.chars().nth(i).unwrap()
                || trans_bin2.chars().nth(i).unwrap() == trans_bin3.chars().nth(i).unwrap()
        )
    }
    for i in 0..stretch_bin1.len() {
        assert!(
            stretch_bin1.chars().nth(i).unwrap() == stretch_bin3.chars().nth(i).unwrap()
                || stretch_bin2.chars().nth(i).unwrap() == stretch_bin3.chars().nth(i).unwrap()
        )
    }
    for i in 0..weight_bins1.len() {
        for j in 0..weight_bins1[i].len() {
            assert!(
                weight_bins1[i].chars().nth(j).unwrap() == weight_bins3[i].chars().nth(j).unwrap()
                    || weight_bins2[i].chars().nth(j).unwrap()
                        == weight_bins3[i].chars().nth(j).unwrap()
            )
        }
    }
    for i in 0..sens_weight_bins1.len() {
        for j in 0..sens_weight_bins1[i].len() {
            assert!(
                sens_weight_bins1[i].chars().nth(j).unwrap()
                    == sens_weight_bins3[i].chars().nth(j).unwrap()
                    || sens_weight_bins2[i].chars().nth(j).unwrap()
                        == sens_weight_bins3[i].chars().nth(j).unwrap()
            )
        }
    }
}

#[test]
fn leg_chrom_conversion() {
    for _ in 0..10 {
        let leg = make_rand_leg(5);
        let chrom = LegChrom::from(&leg);
        let leg2 = Leg::from(&chrom);
        assert_eq!(leg, leg2);
    }
}

#[test]
fn neuron_chrom_conversion() {
    for _ in 0..10 {
        let neuron = make_rand_neuron(5, 5);
        let chrom = NeuronChrom::from(&neuron);
        let neuron2 = Neuron::from(&chrom);
        assert_eq!(neuron, neuron2);
    }
}
