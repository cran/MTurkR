credentials <-
function (keypair) 
{
    if (!missing(keypair)) 
        .value <<- keypair
    else .value
}
