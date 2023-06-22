# probabilistic-reversal-learning-test-analysis
### Code in R used for analyzing data from probabilistic reversal learning (PRL) test performed in operant conditioning boxes. The script analyzes data files from MED-PC IV software.

## PRL test details:
#### Every session contains 200 trials
#### 80% of probability receiving actual feedback after correct (positive reinforcement) or incorrect (negative reinforcement) choice
#### 20% of probability receiving misleading feedback after correct (positive reinforcement) or incorrect (negative reinforcement) choice
#### After every 8 consecutive correct choices (regardless of the outcome) the previously correct choice became incorrect, and vice versa

## Limitations:
#### Be aware that code analysis-only sessions are recorded as "PRL_test_80_20_5s_ITI_Pellet_fan" since this is the name of the MED-PC IV program design and is used in our lab
#### To calculate results separately for each session running on the same test day you need to define in MED-PC software a different subject name for each session
