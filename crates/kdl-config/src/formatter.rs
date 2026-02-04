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

        while pos < bytes.len() && depth > 0 {
            match bytes[pos] {
                b'"' if !in_string => in_string = true,
                b'"' if in_string => in_string = false,
                b'{' if !in_string => depth += 1,
                b'}' if !in_string => depth -= 1,
                _ => {}
            }
            if depth > 0 {
                pos += 1;
            }
        }

        if depth == 0 {
            Some(pos)
        } else {
            None
        }
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
    fn remove_block() {
        let input = "config {\n    name \"demo\"\n}\nother {}\n";
        let output = KdlFormatter::replace_block(input, "config", None);
        assert!(!output.contains("config {"));
        assert!(output.contains("other {}"));
    }
}
