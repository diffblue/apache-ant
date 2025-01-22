package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class CCLockDiffblueTest {
  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link CCLock} (default constructor) Comment is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenCCLockCommentIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCLock ccLock = new CCLock();
    ccLock.setViewPath(null);
    ccLock.setFailOnErr(false);
    ccLock.setNusers(null);
    ccLock.setComment("foo");
    ccLock.setReplace(false);
    ccLock.setObsolete(false);
    ccLock.setObjSel(null);
    ccLock.setPname(null);
    ccLock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link CCLock} (default constructor) Nusers is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenCCLockNusersIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCLock ccLock = new CCLock();
    ccLock.setViewPath(null);
    ccLock.setFailOnErr(false);
    ccLock.setNusers("foo");
    ccLock.setComment(null);
    ccLock.setReplace(false);
    ccLock.setObsolete(false);
    ccLock.setObjSel(null);
    ccLock.setPname(null);
    ccLock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link CCLock} (default constructor) Obsolete is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenCCLockObsoleteIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCLock ccLock = new CCLock();
    ccLock.setViewPath(null);
    ccLock.setFailOnErr(false);
    ccLock.setNusers(null);
    ccLock.setComment(null);
    ccLock.setReplace(false);
    ccLock.setObsolete(true);
    ccLock.setObjSel(null);
    ccLock.setPname(null);
    ccLock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link CCLock} (default constructor) Pname is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenCCLockPnameIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCLock ccLock = new CCLock();
    ccLock.setViewPath(null);
    ccLock.setFailOnErr(false);
    ccLock.setNusers(null);
    ccLock.setComment(null);
    ccLock.setReplace(false);
    ccLock.setObsolete(false);
    ccLock.setObjSel(null);
    ccLock.setPname(null);
    ccLock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link CCLock} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenCCLockProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    CCLock ccLock = new CCLock();
    ccLock.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link CCLock} (default constructor) Replace is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenCCLockReplaceIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCLock ccLock = new CCLock();
    ccLock.setViewPath(null);
    ccLock.setFailOnErr(false);
    ccLock.setNusers(null);
    ccLock.setComment(null);
    ccLock.setReplace(true);
    ccLock.setObsolete(false);
    ccLock.setObjSel(null);
    ccLock.setPname(null);
    ccLock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link CCLock} (default constructor) ViewPath is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenCCLockViewPathIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    CCLock ccLock = new CCLock();
    ccLock.setViewPath("foo");
    ccLock.setFailOnErr(false);
    ccLock.setNusers(null);
    ccLock.setComment(null);
    ccLock.setReplace(false);
    ccLock.setObsolete(false);
    ccLock.setObjSel(null);
    ccLock.setPname(null);
    ccLock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test {@link CCLock#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCLock#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CCLock ccLock = new CCLock();
    ccLock.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> ccLock.execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCLock#setComment(String)}
   *   <li>{@link CCLock#setNusers(String)}
   *   <li>{@link CCLock#setObjSel(String)}
   *   <li>{@link CCLock#setObjselect(String)}
   *   <li>{@link CCLock#setObsolete(boolean)}
   *   <li>{@link CCLock#setPname(String)}
   *   <li>{@link CCLock#setReplace(boolean)}
   *   <li>{@link CCLock#getComment()}
   *   <li>{@link CCLock#getNusers()}
   *   <li>{@link CCLock#getObjselect()}
   *   <li>{@link CCLock#getObsolete()}
   *   <li>{@link CCLock#getPname()}
   *   <li>{@link CCLock#getReplace()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCLock ccLock = new CCLock();

    // Act
    ccLock.setComment("Comment");
    ccLock.setNusers("Nusers");
    ccLock.setObjSel("Objsel");
    ccLock.setObjselect("Objselect");
    ccLock.setObsolete(true);
    ccLock.setPname("Pname");
    ccLock.setReplace(true);
    String actualComment = ccLock.getComment();
    String actualNusers = ccLock.getNusers();
    String actualObjselect = ccLock.getObjselect();
    boolean actualObsolete = ccLock.getObsolete();
    String actualPname = ccLock.getPname();

    // Assert
    assertEquals("Comment", actualComment);
    assertEquals("Nusers", actualNusers);
    assertEquals("Objselect", actualObjselect);
    assertEquals("Pname", actualPname);
    assertTrue(actualObsolete);
    assertTrue(ccLock.getReplace());
  }

  /**
   * Test new {@link CCLock} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCLock}
   */
  @Test
  public void testNewCCLock() {
    // Arrange and Act
    CCLock actualCcLock = new CCLock();

    // Assert
    assertEquals("cleartool", actualCcLock.getClearToolCommand());
    assertNull(actualCcLock.getDescription());
    assertNull(actualCcLock.getTaskName());
    assertNull(actualCcLock.getTaskType());
    assertNull(actualCcLock.getComment());
    assertNull(actualCcLock.getNusers());
    assertNull(actualCcLock.getObjselect());
    assertNull(actualCcLock.getPname());
    assertNull(actualCcLock.getObjSelect());
    assertNull(actualCcLock.getViewPath());
    assertNull(actualCcLock.getProject());
    assertNull(actualCcLock.getOwningTarget());
    assertFalse(actualCcLock.getObsolete());
    assertFalse(actualCcLock.getReplace());
    assertTrue(actualCcLock.getFailOnErr());
  }
}
