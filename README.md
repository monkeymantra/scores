# Building Scores

This application requires:
 - sbt
 - scala 2.13
 
 
## To build   
   
sbt assembly
    

# Running tests

    sbt test
    
# Running the scores


    java -jar target/scala-2.13/scores-assembly-0.1.jar ./scores-input.txt

OR

    cat scores-input.txt | java -jar target/scala-2.13/scores-assembly-0.1.jar ./scores-input.txt
    
