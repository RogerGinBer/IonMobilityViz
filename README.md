# Ionmobility Visualisation 

## Docker build

You can build the container locally with 
`docker build -t ionmobilityviz:latest .`

Note that the very first invocation will download the not-opensource and
not-redistributable `libtimsdata.so`. If you only use your docker image
locally, you could move this into the image build (NYI).

## Docker run 

`docker run --rm -it -p 3838:3838 ionmobilityviz:latest`

Afterwards, you should see you shiny :-) visualisation at http://localhost:3838/IonMobilityViz/

