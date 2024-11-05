use nutype::nutype;
use regex::Regex;
use serde::Deserialize;
use serde::{de, Serialize};
use std::str::FromStr;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize)]
struct Id {
    value: String,
}

macro_rules! serde_id {
    ($type:ident) => {
        impl Serialize for $type {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let id = Id {
                    value: self.to_string(),
                };

                id.serialize(serializer)
            }
        }

        impl<'de> Deserialize<'de> for $type {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                let id = Id::deserialize(deserializer)?;

                Self::from_str(&id.value).map_err(de::Error::custom)
            }
        }
    };
}

#[nutype(derive(Debug, PartialEq, Eq, AsRef, Deref, FromStr))]
pub struct Uid(String);

#[nutype(
    validate(regex = "^([0-2])((\\.0)|(\\.[1-9][0-9]*))*$"),
    derive(Debug, PartialEq, Eq, AsRef, Deref, FromStr)
)]
pub struct IsoOid(String);

#[nutype(
    validate(regex = "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"),
    derive(Debug, PartialEq, Eq, AsRef, Deref, FromStr)
)]
pub struct IsoUuid(String);

#[nutype(
    validate(regex = "^\\w+(\\.\\w+)*$"),
    derive(Debug, PartialEq, Eq, AsRef, Deref, FromStr)
)]
pub struct InternetId(String);

#[nutype(derive(Debug, PartialEq, Eq, AsRef, Deref, FromStr))]
pub struct ObjectId(String);

#[derive(Debug, PartialEq, Eq)]
pub struct UidBasedId {
    pub root: Uid,
    pub extension: Option<String>,
}

impl ToString for UidBasedId {
    fn to_string(&self) -> String {
        let root = self.root.to_string();

        if let Some(ref extension) = self.extension {
            format!("{root}::{extension}")
        } else {
            format!("{root}")
        }
    }
}

impl FromStr for UidBasedId {
    type Err = <Uid as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.splitn(1, "::");

        let root = Uid::from_str(parts.next().unwrap())?;
        let extension = parts.next().map(ToString::to_string);

        let id = UidBasedId { root, extension };

        Ok(id)
    }
}

#[nutype(derive(Debug, PartialEq, Eq, AsRef, Deref, FromStr))]
pub struct HierObjectId(UidBasedId);

#[derive(Debug, PartialEq, Eq)]
pub struct ObjectVersionId {
    pub object_id: Uid,
    pub creating_system_id: Uid,
    pub version_tree_id: VersionTreeId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct VersionTreeId {
    // Trunk version
    pub trunk_version: String,

    // Branch number and version
    pub branch: Option<(u32, u32)>,
}

#[derive(Error, Debug, Clone)]
#[error("invalid VERSION_TREE_ID")]
pub struct InvalidVersionTreeId;

impl FromStr for VersionTreeId {
    type Err = InvalidVersionTreeId;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^([1-9][0-9]*)(?:\.([1-9][0-9]*)\.([1-9][0-9]*))?$").unwrap();

        let caps = re.captures(s).ok_or(InvalidVersionTreeId)?;

        let trunk_version = caps.get(1).unwrap().as_str().to_string();

        let id = if let (Some(branch_number), Some(branch_version)) = (
            caps.get(2).map(|c| c.as_str().parse::<u32>().unwrap()),
            caps.get(3).map(|c| c.as_str().parse::<u32>().unwrap()),
        ) {
            VersionTreeId {
                trunk_version,
                branch: Some((branch_number, branch_version)),
            }
        } else {
            VersionTreeId {
                trunk_version,
                branch: None,
            }
        };

        Ok(id)
    }
}

impl ToString for VersionTreeId {
    fn to_string(&self) -> String {
        let Self {
            trunk_version,
            branch,
        } = self;

        if let Some((branch_number, branch_version)) = branch {
            format!("{trunk_version}.{branch_number}.{branch_version}")
        } else {
            format!("{trunk_version}")
        }
    }
}

serde_id!(Uid);
serde_id!(IsoOid);
serde_id!(IsoUuid);
serde_id!(InternetId);
serde_id!(ObjectId);
serde_id!(UidBasedId);
serde_id!(VersionTreeId);

#[cfg(test)]
mod tests {
    use paste::paste;
    use serde_json::json;

    use super::*;

    macro_rules! serde_tests {
        ($type:ident, $valid_value:literal) => {
            paste! {
                #[test]
                fn [<serialize_ $type:snake>]() {
                    let expected = json!({
                        "value": $valid_value
                    });

                    let value = $type::from_str($valid_value).expect(stringify!($type, " parses"));
                    let serialized = serde_json::to_value(value).expect(stringify!($type, " serializes"));

                    assert_eq!(
                        expected, serialized,
                        stringify!($type, " should serialize as an object")
                    );
                }

                #[test]
                fn [<deserialize_ $type:snake>]() {
                    let expected = $type::from_str($valid_value).expect("OID parses");

                    let json_value = json!({
                        "value": $valid_value
                    });

                    let deserialized: $type = serde_json::from_value(json_value).expect(stringify!($type, " serializes"));

                    assert_eq!(
                        expected, deserialized,
                        stringify!($type, " should deserialize from object")
                    );
                }
            }
        };
    }

    #[test]
    fn parse_iso_oid() {
        IsoOid::from_str("2.16.8").expect("valid OID parses");

        IsoOid::from_str("").expect_err("empty OID does not parse");

        IsoOid::from_str(" 2.16.19502.495 ").expect_err("valid OID with spaces does not parse");

        IsoOid::from_str("2495.16.4.21").expect_err("OID with invalid start digit does not parse");

        IsoOid::from_str("a.b.c.d").expect_err("OID containing letters does not parse");

        IsoOid::from_str("2").expect("OID with single partition parses");
    }

    #[test]
    fn parse_iso_uuid() {
        IsoUuid::from_str("b29f2d41-0451-4658-b5ac-5e09cbdb2be4").expect("valid UUID parses");

        IsoUuid::from_str("").expect_err("empty UUID does not parse");

        IsoUuid::from_str(" b29f2d41-0451-4658-b5ac-5e09cbdb2be4 ")
            .expect_err("valid UUID with spaces does not parse");

        IsoUuid::from_str("b29f2d410451-0451-4658-b5ac-5e09cbdb2be4")
            .expect_err("UUID with invalid pattern does not parse");

        IsoUuid::from_str("B29F2D410451-0451-4658-B5AC-5E09CBDB2BE4")
            .expect_err("UUID containing capital letters does not parse");

        IsoUuid::from_str("2").expect_err("UUID with single character does not parse");
    }

    #[test]
    fn parse_internet_id() {
        InternetId::from_str("org.openehr").expect("valid INTERNET_ID parses");

        InternetId::from_str("$.openehr").expect_err("INTERNET_ID with dollar sign does not parse");

        InternetId::from_str("org.open-ehr").expect_err("INTERNET_ID with dash does not parse");

        InternetId::from_str("org.open ehr").expect_err("INTERNET_ID with space does not parse");

        InternetId::from_str("").expect_err("empty INTERNET_ID does not parse");
    }

    #[test]
    fn parse_uid_based_id() {
        UidBasedId::from_str("root").expect("UID_BASED_ID with only root parses");

        UidBasedId::from_str("root::extension")
            .expect("UID_BASED_ID with root and extension parses");

        UidBasedId::from_str("::")
            .expect("UID_BASED_ID with empty root and empty extension parses");

        UidBasedId::from_str("root::")
            .expect("UID_BASED_ID with non-empty root and empty extension parses");

        UidBasedId::from_str("::extension")
            .expect("UID_BASED_ID with empty root and non-empty extension parses");

        UidBasedId::from_str("").expect("empty UID_BASED_ID parses");
    }

    #[test]
    fn parse_version_tree_id() {
        VersionTreeId::from_str("1").expect("VERSION_TREE_ID with only trunk parses");

        VersionTreeId::from_str("1.2.3")
            .expect("VERSION_TREE_ID with trunk, branch number and branch version parses");

        VersionTreeId::from_str("1.2").expect_err(
            "VERSION_TREE_ID with trunk and branch number but without branch version does not parse",
        );

        VersionTreeId::from_str("1..").expect_err(
            "VERSION_TREE_ID with trunk, empty branch number and empty branch version does not parse",
        );

        VersionTreeId::from_str("1.2.").expect_err(
            "VERSION_TREE_ID with trunk, branch number and empty branch version does not parse",
        );

        VersionTreeId::from_str("1..3").expect_err(
            "VERSION_TREE_ID with trunk, branch version and empty branch number does not parse",
        );

        VersionTreeId::from_str("1..").expect_err(
            "VERSION_TREE_ID with trunk and empty branch number and version does not parse",
        );

        VersionTreeId::from_str("..").expect_err(
            "VERSION_TREE_ID with empty trunk, empty branch number and empty version does not parse",
        );

        VersionTreeId::from_str("0.2.3")
            .expect_err("VERSION_TREE_ID with trunk < 1 does not parse");

        VersionTreeId::from_str("1.0.3")
            .expect_err("VERSION_TREE_ID with branch number < 1 does not parse");

        VersionTreeId::from_str("1.2.0")
            .expect_err("VERSION_TREE_ID with branch version < 1 does not parse");

        VersionTreeId::from_str("").expect_err("empty VERSION_TREE_ID does not parse");
    }

    serde_tests!(IsoOid, "2.16.8");
    serde_tests!(IsoUuid, "b29f2d41-0451-4658-b5ac-5e09cbdb2be4");
    serde_tests!(InternetId, "org.openehr");
    serde_tests!(UidBasedId, "root::extension");
    serde_tests!(VersionTreeId, "1.2.3");
}
