package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class DiagnosticsTaskDiffblueTest {
  /**
   * Test new {@link DiagnosticsTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DiagnosticsTask}
   */
  @Test
  public void testNewDiagnosticsTask() {
    // Arrange and Act
    DiagnosticsTask actualDiagnosticsTask = new DiagnosticsTask();

    // Assert
    Location location = actualDiagnosticsTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDiagnosticsTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualDiagnosticsTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualDiagnosticsTask.getTaskName());
    assertNull(actualDiagnosticsTask.getTaskType());
    assertNull(actualDiagnosticsTask.getProject());
    assertNull(actualDiagnosticsTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualDiagnosticsTask, runtimeConfigurableWrapper.getProxy());
  }
}
