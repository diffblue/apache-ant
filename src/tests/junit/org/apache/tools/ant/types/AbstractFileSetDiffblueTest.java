package org.apache.tools.ant.types;

import static org.junit.Assert.assertFalse;
import org.apache.tools.ant.taskdefs.Sync;
import org.apache.tools.ant.taskdefs.Sync.SyncTarget;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.selectors.AndSelector;
import org.apache.tools.ant.types.selectors.ContainsRegexpSelector;
import org.apache.tools.ant.types.selectors.ContainsSelector;
import org.apache.tools.ant.types.selectors.DateSelector;
import org.apache.tools.ant.types.selectors.DependSelector;
import org.apache.tools.ant.types.selectors.DepthSelector;
import org.apache.tools.ant.types.selectors.DifferentSelector;
import org.apache.tools.ant.types.selectors.ExecutableSelector;
import org.apache.tools.ant.types.selectors.ExtendSelector;
import org.apache.tools.ant.types.selectors.FileSelector;
import org.apache.tools.ant.types.selectors.FilenameSelector;
import org.apache.tools.ant.types.selectors.MajoritySelector;
import org.junit.Test;

public class AbstractFileSetDiffblueTest {
  /**
   * Test {@link AbstractFileSet#addAnd(AndSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addAnd(AndSelector)}
   */
  @Test
  public void testAddAnd() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addAnd(new AndSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addMajority(MajoritySelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addMajority(MajoritySelector)}
   */
  @Test
  public void testAddMajority() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addMajority(new MajoritySelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addDate(DateSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addDate(DateSelector)}
   */
  @Test
  public void testAddDate() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addDate(new DateSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addDifferent(DifferentSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addDifferent(DifferentSelector)}
   */
  @Test
  public void testAddDifferent() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addDifferent(new DifferentSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addFilename(FilenameSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addFilename(FilenameSelector)}
   */
  @Test
  public void testAddFilename() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addFilename(new FilenameSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addCustom(ExtendSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addCustom(ExtendSelector)}
   */
  @Test
  public void testAddCustom() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addCustom(new ExtendSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addContains(ContainsSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addContains(ContainsSelector)}
   */
  @Test
  public void testAddContains() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addContains(new ContainsSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addDepth(DepthSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addDepth(DepthSelector)}
   */
  @Test
  public void testAddDepth() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addDepth(new DepthSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addDepend(DependSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addDepend(DependSelector)}
   */
  @Test
  public void testAddDepend() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addDepend(new DependSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addContainsRegexp(ContainsRegexpSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addContainsRegexp(ContainsRegexpSelector)}
   */
  @Test
  public void testAddContainsRegexp() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addContainsRegexp(new ContainsRegexpSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#addExecutable(ExecutableSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#addExecutable(ExecutableSelector)}
   */
  @Test
  public void testAddExecutable() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.addExecutable(new ExecutableSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }

  /**
   * Test {@link AbstractFileSet#add(FileSelector)}.
   * <p>
   * Method under test: {@link AbstractFileSet#add(FileSelector)}
   */
  @Test
  public void testAdd() {
    // Arrange
    SyncTarget syncTarget = new SyncTarget();

    // Act
    syncTarget.add(new ScriptSelector());

    // Assert
    assertFalse(syncTarget.isChecked());
  }
}
