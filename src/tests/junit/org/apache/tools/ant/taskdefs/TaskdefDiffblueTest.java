package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class TaskdefDiffblueTest {
  /**
   * Test new {@link Taskdef} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Taskdef}
   */
  @Test
  public void testNewTaskdef() {
    // Arrange and Act
    Taskdef actualTaskdef = new Taskdef();

    // Assert
    assertEquals("", actualTaskdef.getURI());
    assertNull(actualTaskdef.getFile());
    assertNull(actualTaskdef.getAntlibClassLoader());
    assertNull(actualTaskdef.getDescription());
    assertNull(actualTaskdef.getTaskName());
    assertNull(actualTaskdef.getTaskType());
    assertNull(actualTaskdef.getClasspathId());
    assertNull(actualTaskdef.getLoaderId());
    assertNull(actualTaskdef.getClassname());
    assertNull(actualTaskdef.getName());
    assertNull(actualTaskdef.getResource());
    assertNull(actualTaskdef.getProject());
    assertNull(actualTaskdef.getOwningTarget());
    assertNull(actualTaskdef.getClasspath());
    assertFalse(actualTaskdef.isReverseLoader());
    assertTrue(actualTaskdef.hasCpDelegate());
  }
}
