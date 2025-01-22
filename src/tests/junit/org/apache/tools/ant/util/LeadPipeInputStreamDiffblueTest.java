package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.IOException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class LeadPipeInputStreamDiffblueTest {
  /**
   * Test {@link LeadPipeInputStream#read()}.
   * <p>
   * Method under test: {@link LeadPipeInputStream#read()}
   */
  @Test
  public void testRead() throws IOException {
    // Arrange
    LeadPipeInputStream leadPipeInputStream = new LeadPipeInputStream(3);
    leadPipeInputStream.setManagingComponent(Path.systemBootClasspath);

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, leadPipeInputStream.read());
  }

  /**
   * Test {@link LeadPipeInputStream#read()}.
   * <ul>
   *   <li>Given {@link LeadPipeInputStream#LeadPipeInputStream(int)} with size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link LeadPipeInputStream#read()}
   */
  @Test
  public void testRead_givenLeadPipeInputStreamWithSizeIsThree() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new LeadPipeInputStream(3)).read());
  }

  /**
   * Test {@link LeadPipeInputStream#read()}.
   * <ul>
   *   <li>Given {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LeadPipeInputStream#read()}
   */
  @Test
  public void testRead_givenSystemBootClasspathProjectIsProject() throws IOException {
    // Arrange
    Path pc = Path.systemBootClasspath;
    pc.setProject(new Project());

    LeadPipeInputStream leadPipeInputStream = new LeadPipeInputStream(3);
    leadPipeInputStream.setManagingComponent(pc);

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, leadPipeInputStream.read());
  }
}
