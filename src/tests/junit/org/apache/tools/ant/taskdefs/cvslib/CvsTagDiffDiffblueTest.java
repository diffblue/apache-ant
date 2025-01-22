package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.AbstractCvsTask;
import org.apache.tools.ant.taskdefs.AbstractCvsTask.Module;
import org.junit.Test;

public class CvsTagDiffDiffblueTest {
  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Given {@link CvsTagDiff} (default constructor) EndDate is {@code rdiff}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_givenCvsTagDiffEndDateIsRdiff_thenThrowBuildException() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("Package/module must be set.");

    CvsTagDiff cvsTagDiff = new CvsTagDiff();
    cvsTagDiff.setEndDate("rdiff");
    cvsTagDiff.setEndTag("End tag or end date must be set.");
    cvsTagDiff.setStartTag("Start tag or start date must be set.");
    cvsTagDiff.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cvsTagDiff.addModule(m);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsTagDiff.execute());
  }

  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Given {@link CvsTagDiff} (default constructor) Package is {@code Destfile must be set.}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_givenCvsTagDiffPackageIsDestfileMustBeSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("Package/module must be set.");

    CvsTagDiff cvsTagDiff = new CvsTagDiff();
    cvsTagDiff.setPackage("Destfile must be set.");
    cvsTagDiff.addModule(m);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsTagDiff.execute());
  }

  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Given {@link CvsTagDiff} (default constructor) StartDate is {@code End tag or end date must be set.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_givenCvsTagDiffStartDateIsEndTagOrEndDateMustBeSet() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("Package/module must be set.");

    CvsTagDiff cvsTagDiff = new CvsTagDiff();
    cvsTagDiff.setStartDate("End tag or end date must be set.");
    cvsTagDiff.setStartTag("Start tag or start date must be set.");
    cvsTagDiff.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cvsTagDiff.addModule(m);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsTagDiff.execute());
  }

  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Given {@link CvsTagDiff} (default constructor) StartDate is {@code Start tag or start date must be set.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_givenCvsTagDiffStartDateIsStartTagOrStartDateMustBeSet() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("Package/module must be set.");

    CvsTagDiff cvsTagDiff = new CvsTagDiff();
    cvsTagDiff.setStartDate("Start tag or start date must be set.");
    cvsTagDiff.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cvsTagDiff.addModule(m);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsTagDiff.execute());
  }

  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Given {@link CvsTagDiff} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_givenCvsTagDiff_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CvsTagDiff()).execute());
  }

  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Given {@link Module} (default constructor) Name is {@code Package/module must be set.}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_givenModuleNameIsPackageModuleMustBeSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("Package/module must be set.");

    CvsTagDiff cvsTagDiff = new CvsTagDiff();
    cvsTagDiff.addModule(m);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsTagDiff.execute());
  }

  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("Package/module must be set.");

    CvsTagDiff cvsTagDiff = new CvsTagDiff();
    cvsTagDiff.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cvsTagDiff.addModule(m);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsTagDiff.execute());
  }

  /**
   * Test {@link CvsTagDiff#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagDiff#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException2() throws BuildException {
    // Arrange
    Module m = new Module();
    m.setName("Package/module must be set.");

    CvsTagDiff cvsTagDiff = new CvsTagDiff();
    cvsTagDiff.setStartTag("Start tag or start date must be set.");
    cvsTagDiff.setDestFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    cvsTagDiff.addModule(m);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsTagDiff.execute());
  }

  /**
   * Test new {@link CvsTagDiff} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CvsTagDiff}
   */
  @Test
  public void testNewCvsTagDiff() {
    // Arrange and Act
    CvsTagDiff actualCvsTagDiff = new CvsTagDiff();

    // Assert
    assertNull(actualCvsTagDiff.getDest());
    assertNull(actualCvsTagDiff.getPassFile());
    assertNull(actualCvsTagDiff.getDescription());
    assertNull(actualCvsTagDiff.getTaskName());
    assertNull(actualCvsTagDiff.getTaskType());
    assertNull(actualCvsTagDiff.getCommand());
    assertNull(actualCvsTagDiff.getCvsRoot());
    assertNull(actualCvsTagDiff.getCvsRsh());
    assertNull(actualCvsTagDiff.getPackage());
    assertNull(actualCvsTagDiff.getTag());
    assertNull(actualCvsTagDiff.getProject());
    assertNull(actualCvsTagDiff.getOwningTarget());
    assertEquals(0, actualCvsTagDiff.getPort());
  }
}
