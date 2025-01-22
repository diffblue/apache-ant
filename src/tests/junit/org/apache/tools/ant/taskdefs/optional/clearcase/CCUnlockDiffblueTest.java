package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class CCUnlockDiffblueTest {
  /**
   * Test {@link CCUnlock#execute()}.
   * <ul>
   *   <li>Given {@link CCUnlock} (default constructor) Comment is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCUnlock#execute()}
   */
  @Test
  public void testExecute_givenCCUnlockCommentIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCUnlock ccUnlock = new CCUnlock();
    ccUnlock.setViewPath(null);
    ccUnlock.setFailOnErr(false);
    ccUnlock.setObjSelect(null);
    ccUnlock.setPname(null);
    ccUnlock.setComment("foo");
    ccUnlock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccUnlock.execute());
  }

  /**
   * Test {@link CCUnlock#execute()}.
   * <ul>
   *   <li>Given {@link CCUnlock} (default constructor) Pname is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCUnlock#execute()}
   */
  @Test
  public void testExecute_givenCCUnlockPnameIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCUnlock ccUnlock = new CCUnlock();
    ccUnlock.setViewPath(null);
    ccUnlock.setFailOnErr(false);
    ccUnlock.setObjSelect(null);
    ccUnlock.setPname(null);
    ccUnlock.setComment(null);
    ccUnlock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccUnlock.execute());
  }

  /**
   * Test {@link CCUnlock#execute()}.
   * <ul>
   *   <li>Given {@link CCUnlock} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCUnlock#execute()}
   */
  @Test
  public void testExecute_givenCCUnlockProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    CCUnlock ccUnlock = new CCUnlock();
    ccUnlock.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> ccUnlock.execute());
  }

  /**
   * Test {@link CCUnlock#execute()}.
   * <ul>
   *   <li>Given {@link CCUnlock} (default constructor) ViewPath is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCUnlock#execute()}
   */
  @Test
  public void testExecute_givenCCUnlockViewPathIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCUnlock ccUnlock = new CCUnlock();
    ccUnlock.setViewPath("foo");
    ccUnlock.setFailOnErr(false);
    ccUnlock.setObjSelect(null);
    ccUnlock.setPname(null);
    ccUnlock.setComment(null);
    ccUnlock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccUnlock.execute());
  }

  /**
   * Test {@link CCUnlock#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCUnlock#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CCUnlock ccUnlock = new CCUnlock();
    ccUnlock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccUnlock.execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCUnlock#setComment(String)}
   *   <li>{@link CCUnlock#setPname(String)}
   *   <li>{@link CCUnlock#getComment()}
   *   <li>{@link CCUnlock#getPname()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCUnlock ccUnlock = new CCUnlock();

    // Act
    ccUnlock.setComment("Comment");
    ccUnlock.setPname("Pname");
    String actualComment = ccUnlock.getComment();

    // Assert
    assertEquals("Comment", actualComment);
    assertEquals("Pname", ccUnlock.getPname());
  }

  /**
   * Test {@link CCUnlock#setObjselect(String)}.
   * <p>
   * Method under test: {@link CCUnlock#setObjselect(String)}
   */
  @Test
  public void testSetObjselect() {
    // Arrange
    CCUnlock ccUnlock = new CCUnlock();

    // Act
    ccUnlock.setObjselect("Objselect");

    // Assert
    assertEquals("Objselect", ccUnlock.getObjselect());
    assertEquals("Objselect", ccUnlock.getObjSelect());
  }

  /**
   * Test {@link CCUnlock#setObjSel(String)}.
   * <p>
   * Method under test: {@link CCUnlock#setObjSel(String)}
   */
  @Test
  public void testSetObjSel() {
    // Arrange
    CCUnlock ccUnlock = new CCUnlock();

    // Act
    ccUnlock.setObjSel("Objsel");

    // Assert
    assertEquals("Objsel", ccUnlock.getObjselect());
    assertEquals("Objsel", ccUnlock.getObjSelect());
  }

  /**
   * Test {@link CCUnlock#getObjselect()}.
   * <p>
   * Method under test: {@link CCUnlock#getObjselect()}
   */
  @Test
  public void testGetObjselect() {
    // Arrange, Act and Assert
    assertNull((new CCUnlock()).getObjselect());
  }

  /**
   * Test new {@link CCUnlock} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCUnlock}
   */
  @Test
  public void testNewCCUnlock() {
    // Arrange and Act
    CCUnlock actualCcUnlock = new CCUnlock();

    // Assert
    assertEquals("cleartool", actualCcUnlock.getClearToolCommand());
    assertNull(actualCcUnlock.getDescription());
    assertNull(actualCcUnlock.getTaskName());
    assertNull(actualCcUnlock.getTaskType());
    assertNull(actualCcUnlock.getComment());
    assertNull(actualCcUnlock.getObjselect());
    assertNull(actualCcUnlock.getPname());
    assertNull(actualCcUnlock.getObjSelect());
    assertNull(actualCcUnlock.getViewPath());
    assertNull(actualCcUnlock.getProject());
    assertNull(actualCcUnlock.getOwningTarget());
    assertTrue(actualCcUnlock.getFailOnErr());
  }
}
