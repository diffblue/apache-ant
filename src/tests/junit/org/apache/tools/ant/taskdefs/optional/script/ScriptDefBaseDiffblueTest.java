package org.apache.tools.ant.taskdefs.optional.script;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class ScriptDefBaseDiffblueTest {
  /**
   * Test {@link ScriptDefBase#getText()}.
   * <p>
   * Method under test: {@link ScriptDefBase#getText()}
   */
  @Test
  public void testGetText() {
    // Arrange, Act and Assert
    assertNull((new ScriptDefBase()).getText());
  }

  /**
   * Test new {@link ScriptDefBase} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ScriptDefBase}
   */
  @Test
  public void testNewScriptDefBase() {
    // Arrange and Act
    ScriptDefBase actualScriptDefBase = new ScriptDefBase();

    // Assert
    Location location = actualScriptDefBase.getLocation();
    assertNull(location.getFileName());
    assertNull(actualScriptDefBase.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualScriptDefBase.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualScriptDefBase.getTaskName());
    assertNull(actualScriptDefBase.getTaskType());
    assertNull(actualScriptDefBase.getText());
    assertNull(actualScriptDefBase.getProject());
    assertNull(actualScriptDefBase.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualScriptDefBase, runtimeConfigurableWrapper.getProxy());
  }
}
