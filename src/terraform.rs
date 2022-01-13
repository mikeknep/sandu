use itertools::Itertools;
use serde::Deserialize;
use std::error::Error;

pub trait Terraform {
    fn show_plan(&self, planfile: &str) -> Result<TerraformPlan, Box<dyn Error>>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum TerraformAction {
    Create,
    Delete,
}

pub fn parse(json_bytes: &[u8]) -> Result<TerraformPlan, Box<dyn Error>> {
    let tfplan = serde_json::from_slice::<TfPlan>(json_bytes)?;
    Ok(tfplan.into())
}

#[derive(Clone, Debug, PartialEq)]
pub struct TerraformResource {
    pub address: String,
    pub preview: serde_json::Value,
    pub r#type: String,
}

#[derive(Debug, PartialEq)]
pub struct TerraformPlan {
    pub pending_creation: Vec<TerraformResource>,
    pub pending_deletion: Vec<TerraformResource>,
}

impl TerraformPlan {
    pub fn unique_types(&self) -> Vec<String> {
        self.pending_creation
            .iter()
            .chain(self.pending_deletion.iter())
            .map(|resource| resource.r#type.clone())
            .unique()
            .sorted()
            .collect()
    }
}

impl From<TfPlan> for TerraformPlan {
    fn from(tfplan: TfPlan) -> Self {
        let mut pending_creation = vec![];
        let mut pending_deletion = vec![];

        for changing_resource in tfplan.changing_resources {
            match (
                changing_resource.change.actions.len(),
                changing_resource.change.before,
                changing_resource.change.after,
            ) {
                (1, Some(before), None) => {
                    pending_deletion.push(TerraformResource {
                        address: changing_resource.address.clone(),
                        r#type: changing_resource.r#type.clone(),
                        preview: before,
                    });
                }
                (1, None, Some(after)) => {
                    pending_creation.push(TerraformResource {
                        address: changing_resource.address.clone(),
                        r#type: changing_resource.r#type.clone(),
                        preview: after,
                    });
                }
                _ => {}
            }
        }

        TerraformPlan {
            pending_creation,
            pending_deletion,
        }
    }
}

// Terraform's own JSON representation
#[derive(Deserialize)]
struct TfPlan {
    #[serde(rename(deserialize = "resource_changes"))]
    changing_resources: Vec<ChangingResource>,
}

#[derive(Deserialize)]
struct ChangingResource {
    address: String,
    change: Change,
    r#type: String,
}

#[derive(Deserialize)]
struct Change {
    actions: Vec<String>,
    after: Option<serde_json::Value>,
    before: Option<serde_json::Value>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn create_terraform_plan_from_parsed_tfplan() {
        let tfplan = TfPlan {
            changing_resources: vec![
                ChangingResource {
                    address: "example.pending_creation".to_string(),
                    r#type: "example".to_string(),
                    change: Change {
                        actions: vec!["create".to_string()],
                        before: None,
                        after: Some(json!({ "example": "pending_creation" })),
                    },
                },
                ChangingResource {
                    address: "example.pending_deletion".to_string(),
                    r#type: "example".to_string(),
                    change: Change {
                        actions: vec!["delete".to_string()],
                        before: Some(json!({ "example": "pending_deletion" })),
                        after: None,
                    },
                },
                ChangingResource {
                    address: "example.create_and_delete".to_string(),
                    r#type: "example".to_string(),
                    change: Change {
                        actions: vec!["create".to_string(), "delete".to_string()],
                        before: Some(json!({ "example": "create_and_delete" })),
                        after: Some(json!({ "example": "create_and_delete" })),
                    },
                },
            ],
        };

        let expected_terraform_plan = TerraformPlan {
            pending_creation: vec![TerraformResource {
                address: "example.pending_creation".to_string(),
                r#type: "example".to_string(),
                preview: json!({ "example": "pending_creation" }),
            }],
            pending_deletion: vec![TerraformResource {
                address: "example.pending_deletion".to_string(),
                r#type: "example".to_string(),
                preview: json!({ "example": "pending_deletion" }),
            }],
        };

        assert_eq!(expected_terraform_plan, tfplan.into());
    }
}
