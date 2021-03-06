﻿namespace TimeOff

/// API urls shared between client and server.
module ServerUrls =

    [<Literal>]
    let Login = "/api/users/login/"

    [<Literal>]
    let UserBalance = "/api/timeoff/user-balance/"
    
    [<Literal>]
    let UserHistoric = "/api/timeoff/user-historic/"
    
    [<Literal>]
    let SendRequest = "/api/timeoff/request/"