package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ComponentdefDiffblueTest {
  /**
   * Test new {@link Componentdef} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Componentdef}
   */
  @Test
  public void testNewComponentdef() {
    // Arrange and Act
    Componentdef actualComponentdef = new Componentdef();

    // Assert
    assertEquals("", actualComponentdef.getURI());
    assertNull(actualComponentdef.getFile());
    assertNull(actualComponentdef.getAntlibClassLoader());
    assertNull(actualComponentdef.getDescription());
    assertNull(actualComponentdef.getTaskName());
    assertNull(actualComponentdef.getTaskType());
    assertNull(actualComponentdef.getClasspathId());
    assertNull(actualComponentdef.getLoaderId());
    assertNull(actualComponentdef.getClassname());
    assertNull(actualComponentdef.getName());
    assertNull(actualComponentdef.getResource());
    assertNull(actualComponentdef.getProject());
    assertNull(actualComponentdef.getOwningTarget());
    assertNull(actualComponentdef.getClasspath());
    assertFalse(actualComponentdef.isReverseLoader());
    assertTrue(actualComponentdef.hasCpDelegate());
  }
}
