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
}
