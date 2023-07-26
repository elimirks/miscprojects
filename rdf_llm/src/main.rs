use rdf::reader::turtle_parser::TurtleParser;
use rdf::reader::rdf_parser::RdfParser;

fn main() {
    let input = include_str!("../assets/foaf.ttl");
    let mut reader = TurtleParser::from_string(input.to_string());

    match reader.decode() {
        Ok(graph) => {
            println!("{}", graph.count());
            for (prefix, uri) in graph.namespaces().iter() {
                println!("{}, {:?}", prefix, uri);
            }
            let root_uri = graph.namespaces()
                .get(":")
                .expect("No root URI found!");
            let mut eli_uri = root_uri.clone();
            eli_uri.append_resource_path("ElijahMirecki");
            let eli_node = graph.create_uri_node(&eli_uri);

            for &triple in graph.get_triples_with_subject(&eli_node).iter() {
                println!("{:?} {:?} {:?}", triple.subject(), triple.predicate(), triple.object());
            }
        },
        Err(err) => {
            panic!("{}", err);
        },
    }
}
