package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class TypedefDiffblueTest {
  /**
   * Test new {@link Typedef} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Typedef}
   */
  @Test
  public void testNewTypedef() {
    // Arrange and Act
    Typedef actualTypedef = new Typedef();

    // Assert
    assertEquals("", actualTypedef.getURI());
    assertNull(actualTypedef.getFile());
    assertNull(actualTypedef.getAntlibClassLoader());
    Location location = actualTypedef.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTypedef.getDescription());
    assertNull(actualTypedef.getTaskName());
    assertNull(actualTypedef.getTaskType());
    assertNull(actualTypedef.getClassname());
    assertNull(actualTypedef.getName());
    assertNull(actualTypedef.getResource());
    assertNull(actualTypedef.getProject());
    assertNull(actualTypedef.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
