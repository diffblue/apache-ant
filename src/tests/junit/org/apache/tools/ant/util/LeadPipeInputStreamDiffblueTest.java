package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.IOException;
import java.io.PipedOutputStream;
import org.apache.ant.antunit.AntUnit;
import org.apache.tools.ant.ProjectComponent;
import org.junit.Test;

public class LeadPipeInputStreamDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link LeadPipeInputStream#LeadPipeInputStream()}
  *   <li>{@link LeadPipeInputStream#setManagingComponent(ProjectComponent)}
  * </ul>
  */
  @Test
  public void testConstructor() throws IOException {
    // Arrange and Act
    LeadPipeInputStream actualLeadPipeInputStream = new LeadPipeInputStream();
    actualLeadPipeInputStream.setManagingComponent(new AntUnit());

    // Assert
    assertEquals(0, actualLeadPipeInputStream.available());
  }

  /**
   * Methods under test: 
   * 
   * <ul>
   *   <li>{@link LeadPipeInputStream#LeadPipeInputStream(PipedOutputStream)}
   *   <li>{@link LeadPipeInputStream#setManagingComponent(ProjectComponent)}
   * </ul>
   */
  @Test
  public void testConstructor2() throws IOException {
    // Arrange and Act
    LeadPipeInputStream actualLeadPipeInputStream = new LeadPipeInputStream(new PipedOutputStream());
    actualLeadPipeInputStream.setManagingComponent(new AntUnit());

    // Assert
    assertEquals(0, actualLeadPipeInputStream.available());
  }

  /**
   * Method under test: {@link LeadPipeInputStream#LeadPipeInputStream(int)}
   */
  @Test
  public void testConstructor3() throws IOException {
    // Arrange, Act and Assert
    assertEquals(0, (new LeadPipeInputStream(3)).available());
  }

  /**
   * Method under test: {@link LeadPipeInputStream#LeadPipeInputStream(PipedOutputStream, int)}
   */
  @Test
  public void testConstructor4() throws IOException {
    // Arrange, Act and Assert
    assertEquals(0, (new LeadPipeInputStream(new PipedOutputStream(), 3)).available());
  }

  /**
   * Method under test: {@link LeadPipeInputStream#read()}
   */
  @Test
  public void testRead() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new LeadPipeInputStream(3)).read());
  }
}

