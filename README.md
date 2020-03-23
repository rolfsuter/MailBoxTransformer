# MailBoxTransformer
MailBoxTransformer transforms Apple Mail's mailbox exports to the structure of Thunderbirds local folders. This tool is written in F# for .NET Core 3.1. 

## Prerequisites
* .NET Core 3.1

## Building
* `dotnet build src/MailBoxTransformer`

## Run
* `dotnet run --project src/MailBoxTransformer "source folder" "target folder"`