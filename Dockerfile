FROM quay.io/cdis/rstudio:4.2.1

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadb-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libssh2-1-dev \
    libpq-dev \
    libfontconfig1 \
    libfreetype6-dev \
    libpng-dev \
    vim \
    wget \
    openjdk-11-jdk \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set Java environment
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
ENV PATH="$JAVA_HOME/bin:$PATH"

# Install Spark 3.5.0 manually
WORKDIR /opt
RUN wget https://archive.apache.org/dist/spark/spark-3.5.0/spark-3.5.0-bin-hadoop3.tgz && \
    tar -xzf spark-3.5.0-bin-hadoop3.tgz && \
    mv spark-3.5.0-bin-hadoop3 spark-3.5.0 && \
    rm spark-3.5.0-bin-hadoop3.tgz

ENV SPARK_HOME=/opt/spark-3.5.0
ENV PATH="$SPARK_HOME/bin:$PATH"

# Copy app files
WORKDIR /home/rstudio
COPY ui.R server.R global.R app.R ./

# Install pak first
RUN R -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/dev/')"

RUN R -e "pak::pkg_install(c( \
  'tidyverse','dplyr','devtools','formatR','remotes','selectr', \
  'caTools','data.table','purrr','writexl','sparklyr','survival', \
  'survminer','ggpubr','DT','shinythemes','shiny','shinyjs' \
), ask = FALSE)"

RUN groupadd -r -g 1001 user && useradd -r -g user -u 1001 user
WORKDIR /home/user
RUN usermod -a -G users rstudio

RUN echo '#!/usr/bin/with-contenv bash \
          \n## load /etc/environment vars first: \
          \n for line in $( cat /etc/environment ) ; do export $line ; done \
          \n cp /etc/rstudio/rserver.conf /etc/rstudio/rserver.conf.bak \
          \n cat /etc/rstudio/rserver.conf.bak | grep -v www-frame-origin > /etc/rstudio/rserver.conf \
          \n echo www-frame-origin=${WWW_FRAME_ORIGIN} >> /etc/rstudio/rserver.conf \
          \n exec /usr/lib/rstudio-server/bin/rserver --server-daemonize 0' \
          > /etc/services.d/rstudio/run

EXPOSE 8787
CMD ["/init"]