package org.apache.tools.ant.taskdefs.optional.unix;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class SymlinkDiffblueTest {
  /**
   * Test {@link Symlink#init()}.
   * <p>
   * Method under test: {@link Symlink#init()}
   */
  @Test
  public void testInit() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();

    // Act
    symlink.init();

    // Assert
    assertEquals("single", symlink.getAction());
  }

  /**
   * Test {@link Symlink#execute()}.
   * <ul>
   *   <li>Given {@link Symlink} (default constructor) Action is {@code execute}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Symlink#execute()}
   */
  @Test
  public void testExecute_givenSymlinkActionIsExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();
    symlink.setAction("execute");

    // Act and Assert
    assertThrows(BuildException.class, () -> symlink.execute());
  }

  /**
   * Test {@link Symlink#execute()}.
   * <ul>
   *   <li>Given {@link Symlink} (default constructor) FailOnError is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Symlink#execute()}
   */
  @Test
  public void testExecute_givenSymlinkFailOnErrorIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();
    symlink.setFailOnError(true);
    symlink.setAction("single");

    // Act and Assert
    assertThrows(BuildException.class, () -> symlink.execute());
  }

  /**
   * Test {@link Symlink#execute()}.
   * <ul>
   *   <li>Given {@link Symlink} (default constructor) FailOnError is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Symlink#execute()}
   */
  @Test
  public void testExecute_givenSymlinkFailOnErrorIsTrue_thenThrowBuildException2() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();
    symlink.setResource("execute");
    symlink.setFailOnError(true);
    symlink.setAction("single");

    // Act and Assert
    assertThrows(BuildException.class, () -> symlink.execute());
  }

  /**
   * Test {@link Symlink#delete()}.
   * <ul>
   *   <li>Given {@link Symlink} (default constructor) FailOnError is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Symlink#delete()}
   */
  @Test
  public void testDelete_givenSymlinkFailOnErrorIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();
    symlink.setFailOnError(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> symlink.delete());
  }

  /**
   * Test {@link Symlink#delete()}.
   * <ul>
   *   <li>Given {@link Symlink} (default constructor) Link is {@code foo}.</li>
   *   <li>Then {@link Symlink} (default constructor) Action is {@code single}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Symlink#delete()}
   */
  @Test
  public void testDelete_givenSymlinkLinkIsFoo_thenSymlinkActionIsSingle() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();
    symlink.setLink("foo");
    symlink.setFailOnError(false);

    // Act
    symlink.delete();

    // Assert
    assertEquals("single", symlink.getAction());
  }

  /**
   * Test {@link Symlink#delete()}.
   * <ul>
   *   <li>Given {@link Symlink} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Symlink} (default constructor) Action is {@code single}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Symlink#delete()}
   */
  @Test
  public void testDelete_givenSymlinkProjectIsProject_thenSymlinkActionIsSingle() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();
    symlink.setProject(new Project());

    // Act
    symlink.delete();

    // Assert
    assertEquals("single", symlink.getAction());
  }

  /**
   * Test {@link Symlink#delete()}.
   * <ul>
   *   <li>Given {@link Symlink} (default constructor).</li>
   *   <li>Then {@link Symlink} (default constructor) Action is {@code single}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Symlink#delete()}
   */
  @Test
  public void testDelete_givenSymlink_thenSymlinkActionIsSingle() throws BuildException {
    // Arrange
    Symlink symlink = new Symlink();

    // Act
    symlink.delete();

    // Assert
    assertEquals("single", symlink.getAction());
  }

  /**
   * Test new {@link Symlink} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Symlink}
   */
  @Test
  public void testNewSymlink() {
    // Arrange and Act
    Symlink actualSymlink = new Symlink();

    // Assert
    assertEquals("action", actualSymlink.getActionParameterName());
    Location location = actualSymlink.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSymlink.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSymlink.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSymlink.getTaskName());
    assertNull(actualSymlink.getTaskType());
    assertNull(actualSymlink.getAction());
    assertNull(actualSymlink.getProject());
    assertNull(actualSymlink.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSymlink, runtimeConfigurableWrapper.getProxy());
  }
}
