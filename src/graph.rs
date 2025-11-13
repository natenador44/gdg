use std::{cmp::Ordering, collections::HashMap, hash::Hash, io, path::Path, rc::Rc, sync::Arc};

use crate::{parser::Parser, tokens::Token};
use anyhow::{Result, anyhow};
use petgraph::{
    Directed,
    dot::{Config, Dot},
    graph::{DiGraph, Edges, NodeIndex},
    visit::EdgeRef,
};

pub type Identifier = Arc<str>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Relationship {
    DependsOn,
    Overrides,
    OverriddenBy,
    SubtreeFoundHere,
    ContainsSubtreeOf,
    ConstrainedBy,
    Constrains,
}

type GraphInner = DiGraph<Dependency, Relationship>;

const DEPTH_START: usize = 1;

pub struct DependencyGraph {
    inner: GraphInner,
    root_name: Identifier,
}

pub trait HasRelationship {
    fn has_relationship(&self, relationship: &Relationship) -> bool;
}

impl HasRelationship for Relationship {
    fn has_relationship(&self, relationship: &Relationship) -> bool {
        self == relationship
    }
}

impl<F> HasRelationship for F
where
    F: Fn(&Relationship) -> bool,
{
    fn has_relationship(&self, relationship: &Relationship) -> bool {
        self(relationship)
    }
}

impl DependencyGraph {
    pub fn root_name(&self) -> &str {
        &self.root_name
    }

    pub fn get_node(&self, idx: NodeIdx) -> Node<'_> {
        Node {
            dependency: &self.inner[idx.0],
            idx,
        }
    }

    pub fn node_has_dependencies(&self, idx: NodeIdx) -> bool {
        self.children_with_relationship(idx, Relationship::DependsOn)
            .next()
            .is_some()
    }

    pub fn children_with_relationship(
        &self,
        idx: NodeIdx,
        r: impl HasRelationship,
    ) -> impl Iterator<Item = Node<'_>> {
        self.inner
            .edges(idx.0)
            .filter(move |e| r.has_relationship(e.weight()))
            .map(|e| Node {
                dependency: &self.inner[e.target()],
                idx: NodeIdx(e.target()),
            })
    }
}

pub struct TraversableGraph<'a> {
    parents: Vec<NodeIndex>,
    cur_root: NodeIndex,
    graph: &'a GraphInner,
}

pub struct Children<'a>(&'a GraphInner, Edges<'a, Relationship, Directed>);

impl<'a> Iterator for Children<'a> {
    type Item = Node<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.1.next().map(|e| Node {
            dependency: &self.0[e.target()],
            idx: NodeIdx(e.target()),
        })
    }
}

pub struct Node<'a> {
    pub dependency: &'a Dependency,
    pub idx: NodeIdx,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeIdx(NodeIndex);

impl<'a> TraversableGraph<'a> {
    pub fn nodes_with_relationship(
        &self,
        relationship: Relationship,
    ) -> impl Iterator<Item = Node<'a>> {
        self.graph
            .edges(self.cur_root)
            .filter(move |e| *e.weight() == relationship)
            .map(|e| Node {
                dependency: &self.graph[e.target()],
                idx: NodeIdx(e.target()),
            })
    }

    pub fn traverse_to_node(&self, child_idx: NodeIdx) -> TraversableGraph<'a> {
        let mut parents = self.parents.clone();
        parents.push(self.cur_root);
        TraversableGraph {
            parents,
            cur_root: child_idx.0,
            graph: self.graph,
        }
    }
}

impl DependencyGraph {
    pub fn traversable_from_root(&self) -> TraversableGraph<'_> {
        TraversableGraph {
            parents: vec![],
            cur_root: NodeIndex::new(0),
            graph: &self.inner,
        }
    }
    pub fn write_to_file(&self, path: impl AsRef<Path>) -> io::Result<()> {
        let dot = Dot::with_attr_getters(
            &self.inner,
            &[Config::NodeNoLabel],
            &|_, _| String::new(),
            &|_, (_, dep)| {
                let label = match &dep.id {
                    DependencyId::Artifact(a) => format!(
                        "{}:{}{}",
                        &a.group,
                        &a.name,
                        a.version
                            .as_ref()
                            .map(|v| format!(":{v}"))
                            .unwrap_or(String::new())
                    ),
                    DependencyId::Project(name) => format!("project: {name}"),
                };
                format!("label = \"{label}\"")
            },
        );

        std::fs::write(path, format!("{:?}", dot))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum DependencyId {
    Project(Identifier),
    Artifact(Artifact),
}

impl std::fmt::Display for DependencyId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyId::Project(name) => write!(f, "project:{name}"),
            DependencyId::Artifact(artifact) => write!(
                f,
                "{}:{}{}",
                artifact.group,
                artifact.name,
                artifact
                    .version
                    .as_ref()
                    .map(|v| format!(":{v}"))
                    .unwrap_or_default()
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Dependency {
    pub id: DependencyId,
    pub statuses: Vec<DependencyStatus>,
}

#[derive(Debug, Clone)]
pub struct Artifact {
    pub group: Identifier,
    pub name: Identifier,
    pub version: Option<Identifier>,
}

impl PartialEq for Artifact {
    fn eq(&self, other: &Self) -> bool {
        self.group == other.group && self.name == other.name && self.version == other.version
    }
}
impl Eq for Artifact {}
impl Hash for Artifact {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.group.hash(state);
        self.name.hash(state);
        self.version.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DependencyStatus {
    /// c
    /// Dependency constraint
    Constraint,
    /// n
    /// missing dependency
    Unresolved,
    /// *
    /// Repeated transitive dependency
    SubTreeAlreadyListed,
    // Version conflict resolved by gradle
    Override(Override),
}

impl std::fmt::Display for DependencyStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DependencyStatus::Constraint => write!(f, "contraint"),
            DependencyStatus::Unresolved => write!(f, "unresolved"),
            DependencyStatus::SubTreeAlreadyListed => write!(f, "already listed"),
            DependencyStatus::Override(o) => match o {
                Override::Version(v) => write!(f, "overridden by version '{v}'"),
                Override::Project(n) => write!(f, "overridden by project '{n}'"),
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Override {
    Version(Identifier),
    Project(Identifier),
}

fn unexpected_eoi() -> anyhow::Error {
    anyhow!("unexpected EOI")
}

fn unexpected_token(token: &Token) -> anyhow::Error {
    anyhow!("unexpected token: {token:?}")
}

fn gen_key(dep: &Dependency) -> Rc<str> {
    let ov = dep.statuses.iter().find_map(|s| {
        if let DependencyStatus::Override(o) = s {
            Some(o)
        } else {
            None
        }
    });
    let key = match (&dep.id, ov) {
        (DependencyId::Artifact(a), None) => {
            format!(
                "{}:{}{}",
                a.group,
                a.name,
                a.version
                    .as_ref()
                    .map(|v| format!(":{v}"))
                    .unwrap_or(String::new())
            )
        }
        (DependencyId::Artifact(a), Some(o)) => match o {
            Override::Version(v) => format!("{}:{}:{v}", a.group, a.name),
            Override::Project(name) => format!("project({name})"),
        },
        (DependencyId::Project(name), None) => {
            format!("project({name})")
        }
        (DependencyId::Project(name), Some(o)) => match o {
            Override::Version(v) => format!("{name}:{v}"),
            Override::Project(name) => format!("project({name})"),
        },
    };
    Rc::from(key)
}

pub fn create_graph(
    mut parser: Parser,
    root_name: impl Into<Identifier>,
) -> Result<DependencyGraph> {
    let root_name = root_name.into();
    let mut graph = GraphInner::new();
    let root = Dependency {
        id: DependencyId::Project(Identifier::clone(&root_name)),
        statuses: vec![],
    };
    let root_idx = graph.add_node(root);
    let mut parents = nonempty::nonempty![root_idx];

    let mut resolved = HashMap::new();
    let mut listed_subtree_parents = HashMap::new();
    let mut constraints = HashMap::new();
    let mut overrides = Vec::new();

    let mut potential_parent = root_idx;

    loop {
        let Some(next) = parser.next()? else {
            break; // should be an actual EOI here, and it's ok
        };

        if let t @ (Token::VerticalPipe | Token::Plus | Token::Backslash) = next.consume() {
            let depth = if t == Token::VerticalPipe {
                let d = determine_depth(&mut parser)?;
                parser.expect(|t| t == &Token::Plus || t == &Token::Backslash)?;
                d
            } else {
                DEPTH_START
            };

            parser.expect_n(3, |t| t == &Token::HorizontalPipe, "-")?;

            let dep = parse_dependency(&mut parser)?;

            // println!("dependency found: {dep:?}");

            let key = gen_key(&dep);

            let dep_idx = graph.add_node(dep.clone());

            // if only status is constraint, or there are no statuses, it is the "resolved" dependency.
            // if statuses contain override, the "resolved" dependency lives elsewhere (not necessarily listed before), regardless of other statuses. The resolved dependency can be a constraint or a normal dependency.
            // if statuses contain sub tree already listed, its children have already been listed before (this one is a duplicate)
            // if statuses do not contain constraint and sub tree already listed, this is a "source" (i.e. what duplicates duplicate)

            let statuses = &dep.statuses;

            match statuses.len() {
                0 => {
                    listed_subtree_parents.insert(key.clone(), dep_idx);
                    resolved.insert(key, dep_idx);
                }
                1 if statuses[0] == DependencyStatus::Constraint => {
                    resolved.insert(key.clone(), dep_idx);
                    constraints.insert(key, dep_idx);
                }
                1 if statuses[0] == DependencyStatus::SubTreeAlreadyListed => {
                    resolved.insert(key.clone(), dep_idx);

                    let source_idx = *listed_subtree_parents
                        .get(&key)
                        .expect(&format!("listed subtree parents contains {key}"));
                    graph.add_edge(dep_idx, source_idx, Relationship::SubtreeFoundHere);
                    graph.add_edge(source_idx, dep_idx, Relationship::ContainsSubtreeOf);
                }
                _ => {
                    if statuses.iter().all(|s| {
                        s != &DependencyStatus::Constraint // constraints won't list out children
                            && s != &DependencyStatus::SubTreeAlreadyListed
                    }) {
                        listed_subtree_parents.insert(key.clone(), dep_idx);
                    }

                    if statuses.contains(&DependencyStatus::SubTreeAlreadyListed) {
                        // these "source" nodes are always listed before the current dep, so this key should be here.
                        let source_idx = *listed_subtree_parents
                            .get(&key)
                            .expect(&format!("listed subtree parents contains {key}"));
                        graph.add_edge(dep_idx, source_idx, Relationship::SubtreeFoundHere);
                        graph.add_edge(source_idx, dep_idx, Relationship::ContainsSubtreeOf);
                    }

                    if statuses
                        .iter()
                        .any(|s| matches!(s, DependencyStatus::Override(_)))
                    {
                        // if we go from an ambiguous version (like no version specified or a version with a wildcard [+]),
                        // then the override just indicates that gradle did some work to resolve the version, and here's what
                        // the version is
                        // TODO make versions smarter
                        if (matches!(&dep.id, DependencyId::Artifact(Artifact { version: Some(v), ..}) if v.contains("+"))
                            || matches!(
                                &dep.id,
                                DependencyId::Artifact(Artifact { version: None, .. })
                            ))
                            && !resolved.contains_key(&key)
                        {
                            resolved.insert(key.clone(), dep_idx);
                        } else {
                            // this dep conflicted with another, and gradle chose the other. It can be anywhere in the tree, so we save these for later and connect them to their "resolved" deps after passing through them all.
                            overrides.push((key.clone(), dep_idx));
                        }
                    }

                    if statuses.contains(&DependencyStatus::Constraint) {
                        constraints.insert(key.clone(), dep_idx);
                    } else if let Some(constrained_by_idx) = constraints.get(&key) {
                        graph.add_edge(dep_idx, *constrained_by_idx, Relationship::ConstrainedBy);
                        graph.add_edge(*constrained_by_idx, dep_idx, Relationship::Constrains);
                    }
                }
            }

            match depth.cmp(&parents.len()) {
                Ordering::Less => {
                    while depth < parents.len() {
                        parents.pop();
                    }

                    graph.add_edge(*parents.last(), dep_idx, Relationship::DependsOn);
                }
                Ordering::Equal => {
                    graph.add_edge(*parents.last(), dep_idx, Relationship::DependsOn);
                }
                Ordering::Greater => {
                    // if depth is greater, the last_idx is its parent, so make that association
                    graph.add_edge(potential_parent, dep_idx, Relationship::DependsOn);
                    // then officially push last_idx to stack since it is a parent for sure now
                    parents.push(potential_parent);
                }
            }
            potential_parent = dep_idx;
        }
    }

    for (dep_key, dep_idx) in overrides {
        // overrides aren't necessarily pointing to an original. If no version is specified in the declaration (build.gradle)
        // but you use something like the spring boot gradle plugin, it will enforce that version and show up as an override
        // so really, the best way to think about these "overrides" is only that gradle did something to resolve them, not
        // necessarily that there was a conflict it had to figure out
        if let Some(resolved_idx) = resolved.get(&dep_key) {
            graph.add_edge(dep_idx, *resolved_idx, Relationship::OverriddenBy);
            graph.add_edge(*resolved_idx, dep_idx, Relationship::Overrides);
        }
    }

    Ok(DependencyGraph {
        inner: graph,
        root_name,
    })
}

fn determine_depth(parser: &mut Parser) -> Result<usize> {
    let mut depth = DEPTH_START;
    loop {
        let Some(next) = parser.next()? else {
            break;
        };

        match next.peek() {
            Token::VerticalPipe => {
                next.consume();
            }
            Token::Indentation => {
                next.consume();
                depth += 1;
            }
            _ => {
                break;
            }
        }
    }
    Ok(depth)
}

fn parse_dependency(parser: &mut Parser) -> Result<Dependency> {
    let ident = parser.expect_map(
        |t| match t {
            Token::Ident(i) => Some(i.clone()),
            _ => None,
        },
        "identifier",
    )?;

    let dep_type = if ident == "project" {
        DependencyId::Project(parse_project(parser)?)
    } else {
        DependencyId::Artifact(parse_artifact(parser, ident)?)
    };

    let statuses = parse_dependency_status(parser)?;

    Ok(Dependency {
        id: dep_type,
        statuses,
    })
}

fn parse_project(parser: &mut Parser) -> Result<Identifier> {
    // we've parsed "project" so far
    let _ = parser.expect(|t| t == &Token::Colon)?;
    let project_name = parser.expect_map(
        |t| match t {
            Token::Ident(i) => Some(Identifier::from(&**i)),
            _ => None,
        },
        "identifier",
    )?;

    Ok(project_name)
}

fn parse_artifact(parser: &mut Parser, group: String) -> Result<Artifact> {
    let remaining_group = parse_group(parser)?;
    let group = group + &remaining_group;
    let _ = parser.expect(|t| t == &Token::Colon)?;
    let name = parse_name(parser)?;
    let next = parser.next()?.ok_or(unexpected_eoi())?;
    let version = match next.peek() {
        Token::Colon => {
            next.consume();
            Some(parse_version(parser)?)
        }
        _ => None,
    };

    Ok(Artifact {
        group: group.into(),
        name,
        version,
    })
}

fn parse_group(parser: &mut Parser) -> Result<Identifier> {
    let mut group = String::new();
    loop {
        let next = parser.next()?.ok_or(unexpected_eoi())?;

        match next.peek() {
            Token::Colon => {
                break;
            }
            other => {
                group += &other.to_string();
            }
        }
        next.consume();
    }

    Ok(group.into())
}

fn parse_name(parser: &mut Parser) -> Result<Identifier> {
    let mut name = String::new();
    loop {
        let next = parser.next()?.ok_or(unexpected_eoi())?;

        match next.peek() {
            Token::Colon | Token::Arrow | Token::ParenOpen => {
                break;
            }
            other => {
                name.push_str(&other.to_string());
            }
        }
        next.consume();
    }

    Ok(name.into())
}
fn parse_version(parser: &mut Parser) -> Result<Identifier> {
    let mut version = String::new();
    loop {
        let next = parser.next()?.ok_or(unexpected_eoi())?;

        match next.peek() {
            Token::NewLine => {
                next.consume();
                break;
            }
            Token::ParenOpen | Token::Arrow => {
                // these are modifiers, we'll deal with those later
                break;
            }
            other => {
                version += &other.to_string();
                next.consume();
            }
        }
    }

    Ok(Identifier::from(version))
}

fn parse_dependency_status(parser: &mut Parser) -> Result<Vec<DependencyStatus>> {
    let mut statuses = Vec::new();

    loop {
        let next = parser.next()?.ok_or(unexpected_eoi())?;

        match next.peek() {
            Token::ParenOpen => {
                next.consume();
                // c, *, n
                let next = parser.next()?.ok_or(unexpected_eoi())?;

                match next.peek() {
                    Token::C => {
                        statuses.push(DependencyStatus::Constraint);
                    }
                    Token::Asterisk => {
                        statuses.push(DependencyStatus::SubTreeAlreadyListed);
                    }
                    Token::N => {
                        statuses.push(DependencyStatus::Unresolved);
                    }
                    other => return Err(unexpected_token(other)),
                }

                next.consume();
                parser.expect(|t| t == &Token::ParenClose)?;

                break; // as far as I can tell, these types of status are the last thing to appear
            }
            Token::Arrow => {
                next.consume();
                statuses.push(DependencyStatus::Override(parse_override(parser)?));
            }
            _ => break,
        }
    }

    Ok(statuses)
}

fn parse_override(parser: &mut Parser) -> Result<Override> {
    let ident = parser.expect_map(
        |t| match t {
            Token::Ident(i) => Some(Identifier::from(&**i)),
            _ => None,
        },
        "identifier",
    )?;

    let ov = if &*ident == "project" {
        Override::Project(parse_project(parser)?)
    } else {
        let remaining_version = parse_version(parser)?;
        let mut full_version = String::with_capacity(ident.len() + remaining_version.len());
        full_version += &*ident;
        full_version += &*remaining_version;
        Override::Version(full_version.into())
    };

    Ok(ov)
}
