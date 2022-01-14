FROM rust:1.58 AS build-env
WORKDIR /app
COPY Cargo.* .
COPY src/ ./src
RUN RUSTFLAGS='-C target-feature=+crt-static' cargo build --release --target x86_64-unknown-linux-gnu


FROM hashicorp/terraform:1.0.9
COPY --from=build-env  /app/target/x86_64-unknown-linux-gnu/release/sandu /usr/local/bin
WORKDIR example
COPY example .
RUN terraform init
ENTRYPOINT ["sandu"]
CMD ["tfplan"]
