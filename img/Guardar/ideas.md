# Workshop

## Can we trust the literature?

There are 2 options:

1) We study effects with more than 90% power, and more than 90% probability of being true.
2) There is MASSIVE publication bias.

![alt text](image-4.png)
<https://x.com/cremieuxrecueil/status/1772765486935056893>

The significance filter, the winner's curse and the need to shrink
![alt text](image-5.png)
 1.1 million z-values from medical research conducted between 1976 and 2019
![alt text](image.png) ![alt text](image-1.png)
<https://doi.org/10.1111/stan.12241>

**Q** What percentage of published findings in psychology are statistically significant?
**A** 96%

![alt text](image-3.png)
<https://doi.org/10.1177/251524592110074>

### Threats to scientific replicability

#### 🧪 1. P-hacking

* Involves manipulating data analysis (e.g. trying different statistical models, selectively removing outliers, or stopping data collection when p < .05) to obtain significant results.
* Leads to inflated Type I error rates and unreliable findings.

#### 💡 2. HARKing

* Formulating hypotheses after seeing the data, and presenting them as a priori.
* Misleads readers and reviewers about the nature of the test and inflates the risk of false positives.

#### 📦 3. Publication Bias

* Journals are more likely to publish positive or novel results than null findings.
* "file drawer problem" creates a biased literature, overestimating effects and underreporting failed replications.

#### 📏 4. Low Statistical Power

* Many studies are underpowered (small sample sizes), increasing the risk of:
  * False negatives (Type II errors)
  * Inflated effect sizes when effects are detected

#### 🔧 5. Flexible Data Analysis Pipelines

* Also known as "researcher degrees of freedom":
  * Multiple ways to preprocess data
  * Different outcome variables
  * Optional covariates
* If not preregistered, these decisions can bias outcomes.

#### 🧠 6. Lack of Preregistration

* Without preregistration, it is difficult to distinguish exploratory from confirmatory analyses.
* Encourages the garden of forking paths (many possible analyses), increasing false positives.

#### 🧾 7. Inadequate Reporting

* Omission of crucial details (e.g. exact methods, software versions, data preprocessing steps) makes replication difficult or impossible.
* Hinders transparency and reproducibility.

#### 📊 8. Selective Outcome Reporting

* Only reporting certain measures or time points based on results.
* Undermines the integrity of confirmatory research.

#### 🔄 9. Failure to Share Data and Code

* Without open access to data and analysis code, replication becomes time-consuming or unfeasible.
* Limits the ability of others to check for errors or reproduce results.

#### 🌐 10. Cultural and Incentive Structures

* “Publish or perish” culture rewards flashy, positive, and novel findings.
* Little reward (until recently) for replication studies or null results.

## Preregistration

<https://www.cos.io/initiatives/prereg>

The value of preregistration for psychological science: A conceptual analysis
<https://doi.org/10.24602/sjpr.62.3_221>

### Key possitive effects

| Positive Effect                                                 | Description                                                                                                                           | Supporting Citations                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| --------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Increased Transparency                                          | Makes research plans, hypotheses, and analyses public, allowing others to verify adherence.                                           | [4](https://consensus.app/papers/an-evaluation-of-the-effectiveness-of-study-dickson-bochantin/ef44d444dfe45026818f66312fa423a4/),[2](https://consensus.app/papers/preregistration-not-a-daunting-practice-stapleton-cousijn/9df464de24a45debb36f32007085dfa9/),[3](https://consensus.app/papers/preregistration-of-mathematical-models-ioannidis/0f1c2c7b812851f39217fa93bec4aba9/),[6](https://consensus.app/papers/harnessing-the-benefits-of-preregistration-for-dezutter-schellekens/80125a4ca0505ed8a8e1ce4b6cedb794/).+2 MORE |
| Reduced Publication and Reporting Bias                          | Encourages publication of all results, not just positive findings, reducing false positives.                                          | [4](https://consensus.app/papers/an-evaluation-of-the-effectiveness-of-study-dickson-bochantin/ef44d444dfe45026818f66312fa423a4/),[2](https://consensus.app/papers/preregistration-not-a-daunting-practice-stapleton-cousijn/9df464de24a45debb36f32007085dfa9/),[3](https://consensus.app/papers/preregistration-of-mathematical-models-ioannidis/0f1c2c7b812851f39217fa93bec4aba9/),[12](https://consensus.app/papers/not-all-preregistrations-are-equal-allen-waldron/4deb08f4a04153f180f6dc51064b4bc7/),+1 MORE                   |
| Clear Distinction Between Exploratory and Confirmatory Research | Helps separate hypothesis-testing from data exploration, improving result interpretation.                                             | [4](https://consensus.app/papers/an-evaluation-of-the-effectiveness-of-study-dickson-bochantin/ef44d444dfe45026818f66312fa423a4/),[6](https://consensus.app/papers/harnessing-the-benefits-of-preregistration-for-dezutter-schellekens/80125a4ca0505ed8a8e1ce4b6cedb794/),[12](https://consensus.app/papers/not-all-preregistrations-are-equal-allen-waldron/4deb08f4a04153f180f6dc51064b4bc7/)                                                                                                                                    |
| Improved Research Quality                                       | Pre-registered studies more often include sample size rationale, analysis plans, and case exclusions, leading to more robust methods. | [4](https://consensus.app/papers/an-evaluation-of-the-effectiveness-of-study-dickson-bochantin/ef44d444dfe45026818f66312fa423a4/),[12](https://consensus.app/papers/not-all-preregistrations-are-equal-allen-waldron/4deb08f4a04153f180f6dc51064b4bc7/),[3](https://consensus.app/papers/preregistration-of-mathematical-models-ioannidis/0f1c2c7b812851f39217fa93bec4aba9/)                                                                                                                                                       |
| Enhanced Credibility and Trust                                  | Increases trust in findings by making deviations from the original plan transparent.                                                  | [3](https://consensus.app/papers/preregistration-of-mathematical-models-ioannidis/0f1c2c7b812851f39217fa93bec4aba9/),[6](https://consensus.app/papers/harnessing-the-benefits-of-preregistration-for-dezutter-schellekens/80125a4ca0505ed8a8e1ce4b6cedb794/),[12](https://consensus.app/papers/not-all-preregistrations-are-equal-allen-waldron/4deb08f4a04153f180f6dc51064b4bc7/),[13](https://consensus.app/papers/preregistration-as-behaviour-developing-an-norris-osborne/f0aee7712a1355d782337e57e2df313e/)                   |
| Smoother Peer Review and Publication Process                    | Reviewers can easily check if the study followed the planned protocol, speeding up review.                                            | [2](https://consensus.app/papers/preregistration-not-a-daunting-practice-stapleton-cousijn/9df464de24a45debb36f32007085dfa9/),[4](https://consensus.app/papers/an-evaluation-of-the-effectiveness-of-study-dickson-bochantin/ef44d444dfe45026818f66312fa423a4/)                                                                                                                                                                                                                                                                   |
| Better Anticipation of Risks and Workflow                       | Encourages thorough planning, leading to more reflective and efficient research processes.                                            | [6](https://consensus.app/papers/harnessing-the-benefits-of-preregistration-for-dezutter-schellekens/80125a4ca0505ed8a8e1ce4b6cedb794/),[13](https://consensus.app/papers/preregistration-as-behaviour-developing-an-norris-osborne/f0aee7712a1355d782337e57e2df313e/)                                                                                                                                                                                                                                                            |

### Preregistration workshop

<https://osf.io/jbpa7>

### Limitations and Considerations

* Pre-registration is not a cure-all; its effectiveness depends on the completeness and specificity of the registered plans. Vague pre-registrations can reintroduce flexibility and bias [12](https://consensus.app/papers/not-all-preregistrations-are-equal-allen-waldron/4deb08f4a04153f180f6dc51064b4bc7/),[10](https://consensus.app/papers/a-systematic-review-of-preregistration-in-autism-research-allchin-linden/5df151aa26fc58c7804430c3d9acbbe6/).
* Some concerns exist about bureaucracy, restriction of exploratory analysis, and potential misuse (e.g., pre-registering after results are known) [4](https://consensus.app/papers/an-evaluation-of-the-effectiveness-of-study-dickson-bochantin/ef44d444dfe45026818f66312fa423a4/),[7](https://consensus.app/papers/campbell-%E2%80%99-s-law-explains-the-replication-crisis-klonsky/2714cea7c1205e86b057e5ee47678208/),[12](https://consensus.app/papers/not-all-preregistrations-are-equal-allen-waldron/4deb08f4a04153f180f6dc51064b4bc7/).
* Not all research types benefit equally, but most empirical studies can gain from some form of pre-registration [6](https://consensus.app/papers/harnessing-the-benefits-of-preregistration-for-dezutter-schellekens/80125a4ca0505ed8a8e1ce4b6cedb794/),[3](https://consensus.app/papers/preregistration-of-mathematical-models-ioannidis/0f1c2c7b812851f39217fa93bec4aba9/),[12](https://consensus.app/papers/not-all-preregistrations-are-equal-allen-waldron/4deb08f4a04153f180f6dc51064b4bc7/).

## Registered Reports

An Excess of Positive Results: Comparing the Standard Psychology Literature With Registered Reports
**![alt text](image-2.png)**
<https://doi.org/10.1177/2515245921100746>  

![alt text](image-6.png)
<https://www.fxstreet.com/analysis/eur-chf-has-stopped-going-down-at-month-end-202503270738>

## Registered Reports 2.0: PCI RR