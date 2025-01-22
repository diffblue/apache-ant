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

public class DirnameDiffblueTest {
  /**
   * Test {@link Dirname#execute()}.
   * <ul>
   *   <li>Given {@link Dirname} (default constructor) File is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Dirname#execute()}
   */
  @Test
  public void testExecute_givenDirnameFileIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    Dirname dirname = new Dirname();
    dirname.setProperty("foo");
    dirname.setFile(null);
    dirname.setProject(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> dirname.execute());
  }

  /**
   * Test {@link Dirname#execute()}.
   * <ul>
   *   <li>Given {@link Dirname} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Dirname#execute()}
   */
  @Test
  public void testExecute_givenDirname_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Dirname()).execute());
  }

  /**
   * Test new {@link Dirname} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Dirname}
   */
  @Test
  public void testNewDirname() {
    // Arrange and Act
    Dirname actualDirname = new Dirname();

    // Assert
    Location location = actualDirname.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDirname.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualDirname.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualDirname.getTaskName());
    assertNull(actualDirname.getTaskType());
    assertNull(actualDirname.getProject());
    assertNull(actualDirname.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualDirname, runtimeConfigurableWrapper.getProxy());
  }
}
