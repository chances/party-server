FROM fpco/stack-build:lts-7.14

ENV LANG en_US.UTF-8

# Install application framework in a separate layer for caching
ONBUILD COPY ./stack-bootstrap .
ONBUILD RUN stack install \
  --resolver lts-$STACK_LTS_VERSION \
  $(cat stack-bootstrap)

# Copy over configuration for building the app
ONBUILD COPY stack.yaml .
ONBUILD COPY *.cabal .

# Build dependencies so that if we change something later we'll have a Docker
# cache of up to this point.
ONBUILD RUN stack build --dependencies-only

ONBUILD COPY . /app/user

# Run pre-build script if it exists (compile CSS, etc)
ONBUILD RUN if [ -x bin/pre-build ]; then bin/pre-build; fi

# Build and copy the executables into the app
ONBUILD RUN stack --local-bin-path=. install

# Clean up
ONBUILD RUN rm -rf /app/user/.stack-work
