//! KDL formatting helpers for updating top-level blocks.

/// Formatter utilities for KDL documents.
pub struct KdlFormatter;

impl KdlFormatter {
    /// Replace a top-level block by name.
    ///
    /// If `body` is `None` or empty, the block is removed.
    /// Otherwise, a new block is formatted with the provided body and inserted.
    pub fn replace_block(content: &str, block_name: &str, body: Option<&str>) -> String {
        let new_block = body
            .map(str::trim)
            .filter(|b| !b.is_empty())
            .map(|b| Self::format_block(block_name, b))
            .unwrap_or_default();

        if let Some((start, end)) = Self::find_block(content, block_name) {
            let mut result = String::with_capacity(content.len() + new_block.len());
            result.push_str(&content[..start]);
            if !new_block.is_empty() {
                result.push_str(&new_block);
            }
            result.push_str(&content[end..]);
            result
        } else if !new_block.is_empty() {
            let mut result = content.to_string();
            if !result.ends_with('\n') {
                result.push('\n');
            }
            result.push('\n');
            result.push_str(&new_block);
            result
        } else {
            content.to_string()
        }
    }

    /// Format a block with the provided body, indenting lines by four spaces.
    pub fn format_block(block_name: &str, body: &str) -> String {
        let mut lines = Vec::new();
        lines.push(format!("{block_name} {{"));

        for line in body.lines() {
            if line.trim().is_empty() {
                lines.push(String::new());
            } else {
                lines.push(format!("    {line}"));
            }
        }

        lines.push("}".to_string());
        lines.join("\n")
    }

    /// Find a top-level block in KDL content.
    ///
    /// Returns (start, end) byte offsets of the entire block including the node name.
    pub fn find_block(content: &str, block_name: &str) -> Option<(usize, usize)> {
        let pattern = format!("{block_name} {{");
        let mut search_start = 0;

        while let Some(rel_pos) = content[search_start..].find(&pattern) {
            let pos = search_start + rel_pos;
            let at_line_start = pos == 0 || content[..pos].ends_with('\n');
            let trimmed_start = content[..pos].rfind('\n').map(|n| n + 1).unwrap_or(0);
            let prefix = &content[trimmed_start..pos];
            let only_whitespace = prefix.chars().all(|c| c.is_whitespace());

            if at_line_start || only_whitespace {
                let block_start = trimmed_start;
                if let Some(end) = Self::find_matching_brace(content, pos + pattern.len() - 1) {
                    let mut block_end = end + 1;
                    if content[block_end..].starts_with('\n') {
                        block_end += 1;
                    }
                    return Some((block_start, block_end));
                }
            }

            search_start = pos + 1;
        }

        None
    }

    /// Find the matching closing brace for an opening brace.
    fn find_matching_brace(content: &str, open_pos: usize) -> Option<usize> {
        let bytes = content.as_bytes();
        if bytes.get(open_pos) != Some(&b'{') {
            return None;
        }

        let mut depth = 1;
        let mut pos = open_pos + 1;
        let mut in_string = false;
        let mut raw_hashes: Option<usize> = None;
        let mut in_line_comment = false;
        let mut in_block_comment = false;

        while pos < bytes.len() && depth > 0 {
            if in_line_comment {
                if bytes[pos] == b'\n' {
                    in_line_comment = false;
                }
                pos += 1;
                continue;
            }

            if in_block_comment {
                if bytes[pos] == b'*' && bytes.get(pos + 1) == Some(&b'/') {
                    in_block_comment = false;
                    pos += 2;
                    continue;
                }
                pos += 1;
                continue;
            }

            if let Some(hashes) = raw_hashes {
                if bytes[pos] == b'"' {
                    let mut matched = true;
                    for offset in 0..hashes {
                        if bytes.get(pos + 1 + offset) != Some(&b'#') {
                            matched = false;
                            break;
                        }
                    }
                    if matched {
                        raw_hashes = None;
                        pos += 1 + hashes;
                        continue;
                    }
                }
                pos += 1;
                continue;
            }

            if in_string {
                if bytes[pos] == b'\\' {
                    pos += 2;
                    continue;
                }
                if bytes[pos] == b'"' {
                    in_string = false;
                    pos += 1;
                    continue;
                }
                pos += 1;
                continue;
            }

            if bytes[pos] == b'/' && bytes.get(pos + 1) == Some(&b'/') {
                in_line_comment = true;
                pos += 2;
                continue;
            }
            if bytes[pos] == b'/' && bytes.get(pos + 1) == Some(&b'*') {
                in_block_comment = true;
                pos += 2;
                continue;
            }

            if bytes[pos] == b'r' {
                let mut idx = pos + 1;
                let mut hashes = 0usize;
                while bytes.get(idx) == Some(&b'#') {
                    hashes += 1;
                    idx += 1;
                }
                if bytes.get(idx) == Some(&b'"') {
                    raw_hashes = Some(hashes);
                    pos = idx + 1;
                    continue;
                }
            }

            if bytes[pos] == b'"' {
                in_string = true;
                pos += 1;
                continue;
            }

            if bytes[pos] == b'{' {
                depth += 1;
                pos += 1;
                continue;
            }
            if bytes[pos] == b'}' {
                depth -= 1;
                if depth == 0 {
                    return Some(pos);
                }
                pos += 1;
                continue;
            }

            pos += 1;
        }

        if depth == 0 { Some(pos) } else { None }
    }
}

#[cfg(test)]
mod tests {
    use super::KdlFormatter;

    #[test]
    fn replace_existing_block() {
        let input = "config {\n    name \"demo\"\n}\n";
        let output = KdlFormatter::replace_block(input, "config", Some("name \"new\""));
        assert!(output.contains("config {\n    name \"new\"\n}"));
    }

    #[test]
    fn append_block_when_missing() {
        let input = "root {}\n";
        let output = KdlFormatter::replace_block(input, "config", Some("name \"demo\""));
        assert!(output.contains("config {\n    name \"demo\"\n}"));
    }

    #[test]
    fn brace_matching_skips_escaped_quotes() {
        let input = "config {\n    name \"he said \\\"hi\\\"\"\n}\n";
        let output = KdlFormatter::replace_block(input, "config", Some("name \"replaced\""));
        assert!(output.contains("config {\n    name \"replaced\"\n}"));
    }

    #[test]
    fn brace_matching_skips_raw_strings() {
        let input = "config {\n    name r#\"{not a block}\"#\n}\n";
        let output = KdlFormatter::replace_block(input, "config", Some("name \"raw\""));
        assert!(output.contains("config {\n    name \"raw\"\n}"));
    }

    #[test]
    fn brace_matching_skips_comments() {
        let input =
            "config {\n    // { comment brace }\n    name \"demo\"\n    /* { block } */\n}\n";
        let output = KdlFormatter::replace_block(input, "config", Some("name \"replaced\""));
        assert!(output.contains("config {\n    name \"replaced\"\n}"));
    }

    #[test]
    fn remove_block() {
        let input = "config {\n    name \"demo\"\n}\nother {}\n";
        let output = KdlFormatter::replace_block(input, "config", None);
        assert!(!output.contains("config {"));
        assert!(output.contains("other {}"));
    }
}
