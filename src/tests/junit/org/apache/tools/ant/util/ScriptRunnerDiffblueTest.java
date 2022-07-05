package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ScriptRunnerDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link ScriptRunner}
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    ScriptRunner actualScriptRunner = new ScriptRunner();

    // Assert
    assertTrue(actualScriptRunner.getBeans().isEmpty());
    assertNull(actualScriptRunner.getScriptClassLoader());
    assertEquals("", actualScriptRunner.getScript());
    assertNull(actualScriptRunner.getProject());
    assertEquals("bsf", actualScriptRunner.getManagerName());
    assertNull(actualScriptRunner.getLanguage());
    assertFalse(actualScriptRunner.getKeepEngine());
    assertFalse(actualScriptRunner.getCompiled());
  }
}

