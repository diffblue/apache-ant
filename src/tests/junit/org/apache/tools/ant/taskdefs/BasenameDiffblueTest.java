package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class BasenameDiffblueTest {
  /**
   * Test {@link Basename#execute()}.
   * <ul>
   *   <li>Given {@link Basename} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Basename#execute()}
   */
  @Test
  public void testExecute_givenBasename_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Basename()).execute());
  }

  /**
   * Test {@link Basename#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Basename#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Basename basename = new Basename();
    basename.setProperty("property attribute required");

    // Act and Assert
    assertThrows(BuildException.class, () -> basename.execute());
  }

  /**
   * Test new {@link Basename} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Basename}
   */
  @Test
  public void testNewBasename() {
    // Arrange and Act
    Basename actualBasename = new Basename();

    // Assert
    Location location = actualBasename.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBasename.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualBasename.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualBasename.getTaskName());
    assertNull(actualBasename.getTaskType());
    assertNull(actualBasename.getProject());
    assertNull(actualBasename.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualBasename, runtimeConfigurableWrapper.getProxy());
  }
}
