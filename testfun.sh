function testfun() {
    file=$1
    echo $file
    shift
    cabal run fun -- $* $file && \
    cabal run yule -- output.core && \
    forge script Output.sol
}    
    

function testcore() {
    echo $1
    cabal run yule -- $1 && forge script Output.sol
}    
