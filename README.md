Lantis is a bug tracking system with a board list interface. The main purpose was to have a fun project for web development in Haskell and to use `servant`.

# Rest API

The server REST API is written with [servant](https://github.com/drever/lantis.git).

# Persistency backend

The persistency backend is a simple file system stoarage. Issues and project are stored in YAML files. It is planned to use git to store revisions of the data. This is inspired by [gitit](https://github.com/drever/lantis.git). Issues are rendered in Markdown on the server side with the [pandoc](https://github.com/drever/lantis.git) library.

# Frontend
The frontend is kept simple. The data is handles with [jQuery](https://github.com/drever/lantis.git). The card lists are rendered as divs and drag and drop is implemented with HTML5.

