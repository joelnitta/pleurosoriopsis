# Pleurosoriopsis

Code repository and raw data for Ebihara *et al.* 2019. "Growth Dynamics of the Independent Gametophytes of *Pleurorosiopsis makinoi* (Polypodiaceae)" *Bulletin of the National Science Museum Series B (Botany)* 45:77-86.

All code is in `R`. To run all analyses and reproduce the manuscript, run `make.R`. 

## Docker image

`make.R` requires various packages to be installed, and may not work properly if package versions have changed. Therefore, a [Docker image is provided](https://hub.docker.com/r/joelnitta/pleurosoriopsis) to run the code reproducibly. To use it, first [install docker](https://docs.docker.com/install/) and [clone this repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository).

Navigate to the cloned repo (where `/path/to/repo` is the path on your machine), and launch the container in the background:

```
cd /path/to/repo
docker-compose up -d
```

Enter the container:

```
docker exec -it pleurosoriopsis_analysis_1 bash
```

Inside the container, run `make.R`:

```
Rscript -e 'source("make.R")'
```