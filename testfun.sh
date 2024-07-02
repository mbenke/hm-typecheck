function testfun() {
    echo $1
    cabal run fun -- -v $1 && \
    cabal run yule -- output.core && \
    forge script Output.sol
}    
    

function testcore() {
    echo $1
    cabal run yule -- $1 && forge script Output.sol
}    
