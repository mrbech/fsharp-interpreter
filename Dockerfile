FROM mcr.microsoft.com/dotnet/core/sdk:2.2

RUN apt-get update
RUN apt-get install -y gnupg git procps

RUN curl -sL https://deb.nodesource.com/setup_11.x | bash -
RUN apt-get install -y nodejs

WORKDIR /
RUN git clone https://github.com/fsprojects/fsharp-language-server
WORKDIR /fsharp-language-server

RUN npm install
RUN dotnet build -c Release

ARG PROJECT_DIR

ADD ./ $PROJECT_DIR/fsharp-interpreter
WORKDIR $PROJECT_DIR/fsharp-interpreter
RUN dotnet restore

CMD dotnet run --project micro-ml
