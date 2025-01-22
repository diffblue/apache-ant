package org.apache.tools.ant.types.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ScriptMapperDiffblueTest {
  /**
   * Test new {@link ScriptMapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ScriptMapper}
   */
  @Test
  public void testNewScriptMapper() {
    // Arrange and Act
    ScriptMapper actualScriptMapper = new ScriptMapper();

    // Assert
    Location location = actualScriptMapper.getLocation();
    assertNull(location.getFileName());
    assertNull(actualScriptMapper.getDescription());
    assertNull(actualScriptMapper.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
