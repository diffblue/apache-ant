package org.apache.tools.ant.types.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class ScriptConditionDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ScriptCondition#setValue(boolean)}
   *   <li>{@link ScriptCondition#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ScriptCondition scriptCondition = new ScriptCondition();

    // Act
    scriptCondition.setValue(true);

    // Assert
    assertTrue(scriptCondition.getValue());
  }

  /**
   * Test new {@link ScriptCondition} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ScriptCondition}
   */
  @Test
  public void testNewScriptCondition() {
    // Arrange and Act
    ScriptCondition actualScriptCondition = new ScriptCondition();

    // Assert
    Location location = actualScriptCondition.getLocation();
    assertNull(location.getFileName());
    assertNull(actualScriptCondition.getDescription());
    assertNull(actualScriptCondition.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualScriptCondition.getValue());
  }
}
