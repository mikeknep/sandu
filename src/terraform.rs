use itertools::Itertools;
use std::error::Error;

pub trait Terraform {
    fn show_plan(&self, planfile: &str) -> Result<TerraformPlan, Box<dyn Error>>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum TerraformAction {
    Create,
    Delete,
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

    pub fn is_empty(&self) -> bool {
        self.pending_creation.len() == 0 && self.pending_deletion.len() == 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use serde_json::json;

    #[test]
    fn plan_is_considered_empty_when_nothing_is_pending_deletion_or_creation() {
        let empty_plan = TerraformPlan {
            pending_creation: vec![],
            pending_deletion: vec![],
        };

        assert!(empty_plan.is_empty());

        let resource = TerraformResource {
            address: "address".to_string(),
            preview: json!({}),
            r#type: "type".to_string(),
        };

        let with_create = TerraformPlan {
            pending_creation: vec![resource.clone()],
            pending_deletion: vec![],
        };
        assert!(!with_create.is_empty());

        let with_delete = TerraformPlan {
            pending_creation: vec![],
            pending_deletion: vec![resource.clone()],
        };
        assert!(!with_delete.is_empty());
    }

    #[test]
    fn list_unique_types_in_plan() {
        let plan = TerraformPlan {
            pending_creation: vec![
                TerraformResource {
                    address: "".to_string(),
                    preview: json!({}),
                    r#type: "a".to_string(),
                },
                TerraformResource {
                    address: "".to_string(),
                    preview: json!({}),
                    r#type: "b".to_string(),
                },
            ],
            pending_deletion: vec![
                TerraformResource {
                    address: "".to_string(),
                    preview: json!({}),
                    r#type: "b".to_string(),
                },
                TerraformResource {
                    address: "".to_string(),
                    preview: json!({}),
                    r#type: "c".to_string(),
                },
            ],
        };

        let unique_types = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        assert_eq!(unique_types, plan.unique_types());
    }
}
