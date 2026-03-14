# Machine Learning for NLP

## Assignment Summary

This project builds and compares two named entity recognition (NER) systems on CONLL-style data with IOB tagging.

## What The Work Does

- Uses token-level labels for entity types PER, ORG, LOC, MISC plus O.
- Loads train/validation/test folds from a compressed JSON dataset.
- Prepares data with BERT subword tokenization and label alignment.
- Trains and evaluates two architectures:
  - LSTM-based sequence tagger (embedding + recurrent + classification layers)
  - Fine-tuned BERT-based tagger
- Compares models with token-level metrics and entity-level quality.

## Main Files

- `projet-2025.ipynb`: full notebook with preprocessing, modeling, training, and evaluation.

## Typical Output

- Trained sequence-labeling models.
- Validation/test metrics and comparative model analysis.
- Qualitative error analysis in the report notebook.
