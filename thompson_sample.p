
from thompson_sampling.bernoulli import BernoulliExperiment
from thompson_sampling.poisson import PoissonExperiment
from thompson_sampling.priors import BetaPrior, GammaPrior
import numpy as np
from scipy import stats
import random as r

TRIALS = 10
EMAILS = 100
actual = {"ad 1": 0.05, "ad 2": 0.1, "ad 3": 0.15}
mu_uniform_sample = round((actual["ad 1"] + actual["ad 2"] + actual["ad 3"]) / 3.0, ndigits=3)


def test(n=10):
    total = 0

    for _ in range(n):
        p1 = GammaPrior()
        p1.add_one(mean=1, variance=None, effective_size=1, label="ad 1")
        p1.add_one(mean=1, variance=None, effective_size=1, label="ad 2")
        p1.add_one(mean=1, variance=None, effective_size=1, label="ad 3")
        experiment = PoissonExperiment(priors=p1)

        exp_total = 0
        choices = [{"ad 1": 0, "ad 2": 0, "ad 3": 0} for _ in range(TRIALS)]
        for t in range(TRIALS):
            for _ in range(EMAILS):
                c = experiment.choose_arm()
                choices[t][c] += 1

            # produce random results
            for c in choices[t].keys():
                result = 0.0
                nhat = choices[t].get(c)

                if nhat != 0:
                    result += np.random.binomial(n=nhat, p=actual.get(c))

                    print(c)
                    print("times sampled: " + str(nhat))
                    print("adjusted response: " + str(round(result / nhat * EMAILS, ndigits=2)))
                    print()
                    experiment.add_rewards([{"label": c, "reward": result / nhat * EMAILS}])
                exp_total += result

            experiment.plot_posterior()

        #print(choices)
        experiment.plot_posterior()
        success_rate = exp_total / (TRIALS * EMAILS)
        total += success_rate

    best_case = max(actual["ad 1"], actual["ad 2"], actual["ad 3"])
    worst_case = min(actual["ad 1"], actual["ad 2"], actual["ad 3"])
    print("Average success rate after " + str(n) + " simulations is " + str(total / n))
    print("Expected success rate for even sampling: " + str(mu_uniform_sample))
    print("This is an: ")
    print("   - " + str(
        round((total / n - mu_uniform_sample) / mu_uniform_sample * 100, ndigits=3))
          + "% increase in effectiveness over uniform sampling")
    print("   - " + str(
        round((total / n - worst_case) / (best_case - worst_case) * 100, ndigits=3)) + "% of best possible strategy")
    print("Best possible case: " + str(best_case))


test(n=10)
