docker kill fom_shiny_server_test_container

docker rm fom_shiny_server_test_container

docker build -t fom_shiny_server_test_build .

docker run --rm -p 3838:3838 \
    -v /Users/sventhies/'OneDrive - Traum-Ferienwohnungen GmbH'/Privat/Sven/FOM/Lehrapps/Sampling/:/srv/shiny-server/ \
    -v /Users/sventhies/'OneDrive - Traum-Ferienwohnungen GmbH'/Privat/Sven/FOM/Lehrapps/docker_testing/shinylogs/:/var/log/shiny-server/ \
    --name fom_shiny_server_test_container fom_shiny_server_test_build