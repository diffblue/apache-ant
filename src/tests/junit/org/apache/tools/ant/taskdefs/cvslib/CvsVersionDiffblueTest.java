package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.AbstractCvsTask;
import org.apache.tools.ant.taskdefs.AbstractCvsTask.Module;
import org.junit.Test;

public class CvsVersionDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CvsVersion#setClientVersionProperty(String)}
   *   <li>{@link CvsVersion#setServerVersionProperty(String)}
   *   <li>{@link CvsVersion#getClientVersion()}
   *   <li>{@link CvsVersion#getServerVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CvsVersion cvsVersion = new CvsVersion();

    // Act
    cvsVersion.setClientVersionProperty("1.0.2");
    cvsVersion.setServerVersionProperty("1.0.2");
    String actualClientVersion = cvsVersion.getClientVersion();

    // Assert
    assertNull(actualClientVersion);
    assertNull(cvsVersion.getServerVersion());
  }

  /**
   * Test {@link CvsVersion#supportsCvsLogWithSOption()}.
   * <p>
   * Method under test: {@link CvsVersion#supportsCvsLogWithSOption()}
   */
  @Test
  public void testSupportsCvsLogWithSOption() {
    // Arrange, Act and Assert
    assertFalse((new CvsVersion()).supportsCvsLogWithSOption());
  }

  /**
   * Test {@link CvsVersion#execute()}.
   * <ul>
   *   <li>Given {@link CvsVersion} (default constructor) ClientVersionProperty is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsVersion#execute()}
   */
  @Test
  public void testExecute_givenCvsVersionClientVersionPropertyIsFoo() {
    // Arrange
    CvsVersion cvsVersion = new CvsVersion();
    cvsVersion.setClientVersionProperty("foo");
    cvsVersion.setServerVersionProperty(null);
    cvsVersion.setProject(new Project());

    // Act
    cvsVersion.execute();

    // Assert
    File dest = cvsVersion.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertEquals("version", cvsVersion.getCommand());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvsVersion.getProject().getBaseDir());
  }

  /**
   * Test {@link CvsVersion#execute()}.
   * <ul>
   *   <li>Given {@link CvsVersion} (default constructor) ClientVersionProperty is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsVersion#execute()}
   */
  @Test
  public void testExecute_givenCvsVersionClientVersionPropertyIsNull() {
    // Arrange
    CvsVersion cvsVersion = new CvsVersion();
    cvsVersion.setClientVersionProperty(null);
    cvsVersion.setServerVersionProperty(null);
    cvsVersion.setProject(new Project());

    // Act
    cvsVersion.execute();

    // Assert
    File dest = cvsVersion.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertEquals("version", cvsVersion.getCommand());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvsVersion.getProject().getBaseDir());
  }

  /**
   * Test {@link CvsVersion#execute()}.
   * <ul>
   *   <li>Given {@link CvsVersion} (default constructor) ServerVersionProperty is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsVersion#execute()}
   */
  @Test
  public void testExecute_givenCvsVersionServerVersionPropertyIsFoo() {
    // Arrange
    CvsVersion cvsVersion = new CvsVersion();
    cvsVersion.setClientVersionProperty(null);
    cvsVersion.setServerVersionProperty("foo");
    cvsVersion.setProject(new Project());

    // Act
    cvsVersion.execute();

    // Assert
    File dest = cvsVersion.getDest();
    assertEquals("apache-ant-1.10.15", dest.getName());
    assertEquals("version", cvsVersion.getCommand());
    assertTrue(dest.isAbsolute());
    assertSame(dest, cvsVersion.getProject().getBaseDir());
  }

  /**
   * Test {@link CvsVersion#execute()}.
   * <ul>
   *   <li>Then {@link CvsVersion} (default constructor) Dest Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsVersion#execute()}
   */
  @Test
  public void testExecute_thenCvsVersionDestNameIsTestTxt() {
    // Arrange
    Module m = new Module();
    m.setName("version");

    CvsVersion cvsVersion = new CvsVersion();
    cvsVersion.setDest(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cvsVersion.addModule(m);

    // Act
    cvsVersion.execute();

    // Assert
    File dest = cvsVersion.getDest();
    assertEquals("test.txt", dest.getName());
    assertEquals("version", cvsVersion.getCommand());
    assertTrue(dest.isAbsolute());
  }

  /**
   * Test new {@link CvsVersion} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CvsVersion}
   */
  @Test
  public void testNewCvsVersion() {
    // Arrange and Act
    CvsVersion actualCvsVersion = new CvsVersion();

    // Assert
    assertNull(actualCvsVersion.getDest());
    assertNull(actualCvsVersion.getPassFile());
    assertNull(actualCvsVersion.getDescription());
    assertNull(actualCvsVersion.getTaskName());
    assertNull(actualCvsVersion.getTaskType());
    assertNull(actualCvsVersion.getCommand());
    assertNull(actualCvsVersion.getCvsRoot());
    assertNull(actualCvsVersion.getCvsRsh());
    assertNull(actualCvsVersion.getPackage());
    assertNull(actualCvsVersion.getTag());
    assertNull(actualCvsVersion.getClientVersion());
    assertNull(actualCvsVersion.getServerVersion());
    assertNull(actualCvsVersion.getProject());
    assertNull(actualCvsVersion.getOwningTarget());
    assertEquals(0, actualCvsVersion.getPort());
  }
}
