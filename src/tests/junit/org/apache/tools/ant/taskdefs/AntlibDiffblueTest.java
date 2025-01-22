package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class AntlibDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Antlib}
   *   <li>{@link Antlib#setClassLoader(ClassLoader)}
   *   <li>{@link Antlib#setURI(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Antlib actualAntlib = new Antlib();
    actualAntlib.setClassLoader(new AntClassLoader());
    actualAntlib.setURI("Uri");

    // Assert
    Location location = actualAntlib.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAntlib.getDescription());
    assertNull(actualAntlib.getTaskName());
    assertNull(actualAntlib.getTaskType());
    assertNull(actualAntlib.getProject());
    assertNull(actualAntlib.getOwningTarget());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
