cd ../lands-java-lib/
mvn deploy
mvn install package
cd ../civs/
mvn -U dependency:resolve
