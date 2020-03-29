# MailBoxTransformer
MailBoxTransformer transforms Apple Mail's mailbox exports to the structure of Thunderbirds local folders. This tool is written in F# for .NET Core 3.1. 

## Prerequisites
* [.NET Core 3.1](https://dotnet.microsoft.com/download ".NET Core download page")

## Building
* Run in the Terminal:
* `dotnet build src/MailBoxTransformer`

## Run
* Run in the Terminal:
* `dotnet run --project src/MailBoxTransformer "source folder" "target folder"`

## Credits
* Tree datastructure and program architecture inspired and adopted from
  * [ploeh/picture-archivist](https://github.com/ploeh/picture-archivist) – Copyright © 2018 Mark Seemann
  * Mark Seemann: ['Picture archivist in F#'](https://blog.ploeh.dk/2019/09/16/picture-archivist-in-f/) – Copyright © 2018 Mark Seemann 
* Inspired by 
  * Scott Wlaschin: ['Trees in the real world'](https://fsharpforfunandprofit.com/posts/recursive-types-and-folds-3b/) – Copyright © 2015 fsharpforfunandprofit.com / Scott Wlaschin

## License
* MailBoxTransformer is licensed under the [MIT](LICENSE.txt) license.
* Copyright © 2020 [Rolf Suter](https://rolfsuter.ch)