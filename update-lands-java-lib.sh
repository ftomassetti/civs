cd ../lands-java-lib/
mvn install package
cd ../civs/
mvn -U dependency:resolve
