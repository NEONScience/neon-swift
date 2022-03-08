# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.0.5

# create an R user
ENV USER rstudio

# use packagemanager.rstudio.com to determine necessary non-R system prerequisites to install 
# e.g. https://packagemanager.rstudio.com/client/#/repos/2/packages/httr
RUN apt-get update
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y make 
RUN apt-get install -y libicu-dev 
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y pandoc
RUN apt-get install -y libssl-dev 
RUN apt-get install -y libxml2-dev 
RUN apt-get install -y libicu-dev

copy /eddycopipe_0.2.2.tar.gz /tmp/
COPY ./docker_r_lib_install.R /tmp/requirements.R 
## install required libs on container
RUN Rscript /tmp/requirements.R

## Copy your working files over
## The $USER defaults to `rstudio` but you can change this at runtime
COPY . / /srv/shiny-server/swift/

RUN chmod 777 /srv/shiny-server/swift/data/user_log/user_log.RDS

# COPY ./data /home/$USER/Data

CMD ["/usr/bin/shiny-server"]