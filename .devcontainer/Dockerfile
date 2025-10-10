# Use a base image with Ubuntu
FROM mcr.microsoft.com/devcontainers/base:ubuntu

# Install R
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    r-base \
    r-base-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Set default shell
SHELL ["/bin/bash", "-c"]
