# *Pleurosoriopsis*

Code repository and raw data for [Ebihara *et al.* 2019. "Growth Dynamics of the Independent Gametophytes of *Pleurorosiopsis makinoi* (Polypodiaceae)" *Bulletin of the National Science Museum Series B (Botany)* 45:77-86.](https://www.kahaku.go.jp/research/publication/botany.html)

## Workflow

All code is in [R](https://cran.r-project.org/). The [targets package](https://docs.ropensci.org/targets/) is used to manage the workflow. To run all analyses and generate the manuscript, [clone this repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository) and run `_targets.R`.

## Reproducible analysis with Docker

`_targets.R` requires various packages to be installed, and may not work properly if package versions have changed. Therefore, a [Docker image is provided](https://hub.docker.com/r/joelnitta/pleurosoriopsis) to run the code reproducibly.

To use it, first [install docker](https://docs.docker.com/install/).

To run the analysis, execute this code in your console:

```
docker run --rm -v ${PWD}:/wd -w /wd joelnitta/pleurosoriopsis:targets \
  bash /tmp/make.sh
```

You will see the targets being built by {targets}, and the final manuscript should be compiled at the end as `pleurosoriopsis/ms.pdf`.

## Interacting with the code

You can run RStudio inside the Docker container.

First run this code in your console (must be run from within the `pleurosoriopsis` folder):

```
docker run --rm -dt -v ${PWD}:/home/rstudio/ \
  -p 8787:8787 \
  -e DISABLE_AUTH=true \
  --name pleuro \
  joelnitta/pleurosoriopsis:targets
```

Then, navigate to <localhost:8787/> in your browser.

In the RStudio "Files" pane, click `Pleurosoriopsis.Rproj` to open the project.

When you are done, stop and remove the container:

```
docker kill pleuro
```