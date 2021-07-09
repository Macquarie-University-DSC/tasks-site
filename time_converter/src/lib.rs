use chrono::prelude::*;
use chrono::LocalResult;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn time_converter(
    year: i32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
) -> Result<i64, JsValue> {
    let dt_result =
        Utc.from_local_datetime(&NaiveDate::from_ymd(year, month, day).and_hms(hour, minute, 0));

    if let LocalResult::Single(dt) = dt_result {
        Ok(dt.timestamp_millis())
    } else {
        Err(JsValue::from_str("Could not convert to posix timestamp."))
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_time_to_posix() -> Result<(), wasm_bindgen::JsValue> {
        use crate::time_to_posix;
        use chrono::prelude::*;

        let lt = Local::now().naive_local();
        let result = time_converter(lt.year(), lt.month(), lt.day(), lt.hour(), lt.minute())?;

        assert!(result != 0);

        Ok(())
    }
}
