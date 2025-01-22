package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.SQLExec.DelimiterType;
import org.apache.tools.ant.taskdefs.SQLExec.OnError;
import org.apache.tools.ant.taskdefs.SQLExec.Transaction;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class SQLExecDiffblueTest {
  /**
   * Test DelimiterType {@link DelimiterType#getValues()}.
   * <p>
   * Method under test: {@link DelimiterType#getValues()}
   */
  @Test
  public void testDelimiterTypeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{DelimiterType.NORMAL, DelimiterType.ROW}, (new DelimiterType()).getValues());
  }

  /**
   * Test DelimiterType new {@link DelimiterType} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link DelimiterType}
   */
  @Test
  public void testDelimiterTypeNewDelimiterType() {
    // Arrange and Act
    DelimiterType actualDelimiterType = new DelimiterType();

    // Assert
    assertNull(actualDelimiterType.getValue());
    assertEquals(-1, actualDelimiterType.getIndex());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SQLExec#setAppend(boolean)}
   *   <li>{@link SQLExec#setCsvColumnSeparator(String)}
   *   <li>{@link SQLExec#setDelimiter(String)}
   *   <li>{@link SQLExec#setEncoding(String)}
   *   <li>{@link SQLExec#setErrorProperty(String)}
   *   <li>{@link SQLExec#setEscapeProcessing(boolean)}
   *   <li>{@link SQLExec#setExpandProperties(boolean)}
   *   <li>{@link SQLExec#setForceCsvQuoteChar(boolean)}
   *   <li>{@link SQLExec#setKeepformat(boolean)}
   *   <li>{@link SQLExec#setOutput(Resource)}
   *   <li>{@link SQLExec#setOutputEncoding(String)}
   *   <li>{@link SQLExec#setPrint(boolean)}
   *   <li>{@link SQLExec#setRawBlobs(boolean)}
   *   <li>{@link SQLExec#setRowCountProperty(String)}
   *   <li>{@link SQLExec#setShowWarnings(boolean)}
   *   <li>{@link SQLExec#setShowheaders(boolean)}
   *   <li>{@link SQLExec#setShowtrailers(boolean)}
   *   <li>{@link SQLExec#setSrc(File)}
   *   <li>{@link SQLExec#setStrictDelimiterMatching(boolean)}
   *   <li>{@link SQLExec#setTreatWarningsAsErrors(boolean)}
   *   <li>{@link SQLExec#setWarningProperty(String)}
   *   <li>{@link SQLExec#getExpandProperties()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act
    sqlExec.setAppend(true);
    sqlExec.setCsvColumnSeparator("foo");
    sqlExec.setDelimiter("Delimiter");
    sqlExec.setEncoding(Manifest.JAR_ENCODING);
    sqlExec.setErrorProperty("An error occurred");
    sqlExec.setEscapeProcessing(true);
    sqlExec.setExpandProperties(true);
    sqlExec.setForceCsvQuoteChar(true);
    sqlExec.setKeepformat(true);
    sqlExec.setOutput(new Resource());
    sqlExec.setOutputEncoding(Manifest.JAR_ENCODING);
    sqlExec.setPrint(true);
    sqlExec.setRawBlobs(true);
    sqlExec.setRowCountProperty("3");
    sqlExec.setShowWarnings(true);
    sqlExec.setShowheaders(true);
    sqlExec.setShowtrailers(true);
    sqlExec.setSrc(Copy.NULL_FILE_PLACEHOLDER);
    sqlExec.setStrictDelimiterMatching(true);
    sqlExec.setTreatWarningsAsErrors(true);
    sqlExec.setWarningProperty("Warning Property");

    // Assert
    assertTrue(sqlExec.getExpandProperties());
  }

  /**
   * Test {@link SQLExec#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SQLExec#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenSQLExec_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SQLExec()).addFileset(null));
  }

  /**
   * Test {@link SQLExec#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SQLExec#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenSQLExec_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SQLExec()).add(null));
  }

  /**
   * Test OnError {@link OnError#getValues()}.
   * <p>
   * Method under test: {@link OnError#getValues()}
   */
  @Test
  public void testOnErrorGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"continue", "stop", "abort"}, (new OnError()).getValues());
  }

  /**
   * Test OnError new {@link OnError} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link OnError}
   */
  @Test
  public void testOnErrorNewOnError() {
    // Arrange and Act
    OnError actualOnError = new OnError();

    // Assert
    assertNull(actualOnError.getValue());
    assertEquals(-1, actualOnError.getIndex());
  }

  /**
   * Test {@link SQLExec#setCsvQuoteCharacter(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SQLExec#setCsvQuoteCharacter(String)}
   */
  @Test
  public void testSetCsvQuoteCharacter_whenFoo_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SQLExec()).setCsvQuoteCharacter("foo"));
  }

  /**
   * Test {@link SQLExec#execute()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor) Src is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SQLExec#execute()}
   */
  @Test
  public void testExecute_givenSQLExecSrcIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    SQLExec sqlExec = new SQLExec();
    sqlExec.setSrc(Copy.NULL_FILE_PLACEHOLDER);
    sqlExec.addText("Source file or resource collection, transactions or sql statement must be set!");

    // Act and Assert
    assertThrows(BuildException.class, () -> sqlExec.execute());
  }

  /**
   * Test {@link SQLExec#execute()}.
   * <ul>
   *   <li>Given {@link SQLExec} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SQLExec#execute()}
   */
  @Test
  public void testExecute_givenSQLExec_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SQLExec()).execute());
  }

  /**
   * Test {@link SQLExec#lastDelimiterPosition(StringBuffer, String)}.
   * <ul>
   *   <li>When {@link StringBuffer#StringBuffer(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SQLExec#lastDelimiterPosition(StringBuffer, String)}
   */
  @Test
  public void testLastDelimiterPosition_whenStringBufferWithEmptyString_thenReturnMinusOne() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act and Assert
    assertEquals(-1, sqlExec.lastDelimiterPosition(new StringBuffer(""), "Current Line"));
  }

  /**
   * Test {@link SQLExec#lastDelimiterPosition(StringBuffer, String)}.
   * <ul>
   *   <li>When {@link StringBuffer#StringBuffer(String)} with {@code foo}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SQLExec#lastDelimiterPosition(StringBuffer, String)}
   */
  @Test
  public void testLastDelimiterPosition_whenStringBufferWithFoo_thenReturnMinusOne() {
    // Arrange
    SQLExec sqlExec = new SQLExec();

    // Act and Assert
    assertEquals(-1, sqlExec.lastDelimiterPosition(new StringBuffer("foo"), "Current Line"));
  }

  /**
   * Test new {@link SQLExec} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SQLExec}
   */
  @Test
  public void testNewSQLExec() {
    // Arrange and Act
    SQLExec actualSqlExec = new SQLExec();

    // Assert
    assertNull(actualSqlExec.getDescription());
    assertNull(actualSqlExec.getTaskName());
    assertNull(actualSqlExec.getTaskType());
    assertNull(actualSqlExec.getPassword());
    assertNull(actualSqlExec.getRdbms());
    assertNull(actualSqlExec.getUrl());
    assertNull(actualSqlExec.getUserId());
    assertNull(actualSqlExec.getVersion());
    assertNull(actualSqlExec.getLoader());
    assertNull(actualSqlExec.getProject());
    assertNull(actualSqlExec.getOwningTarget());
    assertNull(actualSqlExec.getClasspath());
    assertFalse(actualSqlExec.isAutocommit());
    assertTrue(actualSqlExec.getExpandProperties());
  }

  /**
   * Test Transaction {@link Transaction#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Transaction#addConfigured(ResourceCollection)}
   */
  @Test
  public void testTransactionAddConfigured_whenFileList_thenThrowBuildException() {
    // Arrange
    Transaction transaction = (new SQLExec()).new Transaction();

    // Act and Assert
    assertThrows(BuildException.class, () -> transaction.addConfigured(new FileList()));
  }

  /**
   * Test Transaction {@link Transaction#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Transaction#addConfigured(ResourceCollection)}
   */
  @Test
  public void testTransactionAddConfigured_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new SQLExec()).new Transaction()).addConfigured(Resources.NONE));
  }

  /**
   * Test Transaction {@link Transaction#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and path is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Transaction#addConfigured(ResourceCollection)}
   */
  @Test
  public void testTransactionAddConfigured_whenPathWithPIsProjectAndPathIsNull() {
    // Arrange
    Transaction transaction = (new SQLExec()).new Transaction();

    // Act and Assert
    assertThrows(BuildException.class, () -> transaction.addConfigured(new Path(new Project(), null)));
  }

  /**
   * Test Transaction {@link Transaction#setSrc(File)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Transaction#setSrc(File)}
   */
  @Test
  public void testTransactionSetSrc_thenThrowBuildException() {
    // Arrange
    Transaction transaction = (new SQLExec()).new Transaction();
    transaction.setSrcResource(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> transaction.setSrc(Copy.NULL_FILE_PLACEHOLDER));
  }
}
